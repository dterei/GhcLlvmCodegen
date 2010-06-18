-- ----------------------------------------------------------------------------
-- | Base LLVM Code Generation module
--
-- Contains functions useful through out the code generator.
--

module LlvmCodeGen.Base (

        LlvmCmmTop, LlvmBasicBlock,
        LlvmUnresData, LlvmData, UnresLabel, UnresStatic,

        LlvmEnv, initLlvmEnv, clearVars, varLookup, varInsert,
        funLookup, funInsert,

        cmmToLlvmType, widthToLlvmFloat, widthToLlvmInt, llvmFunTy,
        llvmFunSig, llvmStdFunAttrs, llvmFunAlign, llvmInfAlign,
        llvmPtrBits, llvmGhcCC,

        strCLabel_llvm, genCmmLabelRef, genStringLabelRef

    ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Regs

import CgUtils ( activeStgRegs )
import CLabel
import Cmm

import FastString
import qualified Outputable as Outp
import Unique
import UniqFM

-- ----------------------------------------------------------------------------
-- * Some Data Types
--

type LlvmCmmTop = GenCmmTop LlvmData [CmmStatic] (ListGraph LlvmStatement)
type LlvmBasicBlock = GenBasicBlock LlvmStatement

-- | Unresolved code.
-- Of the form: (data label, data type, unresovled data)
type LlvmUnresData = (CLabel, LlvmType, [UnresStatic])

-- | Top level LLVM Data (globals and type aliases)
type LlvmData = ([LMGlobal], [LlvmType])

-- | An unresolved Label.
--
-- Labels are unresolved when we haven't yet determined if they are defined in
-- the module we are currently compiling, or an external one.
type UnresLabel  = CmmLit
type UnresStatic = Either UnresLabel LlvmStatic

-- ----------------------------------------------------------------------------
-- * Type translations
--

-- | Translate a basic CmmType to an LlvmType.
cmmToLlvmType :: CmmType -> LlvmType
cmmToLlvmType ty | isFloatType ty = widthToLlvmFloat $ typeWidth ty
                 | otherwise      = widthToLlvmInt   $ typeWidth ty

-- | Translate a Cmm Float Width to a LlvmType.
widthToLlvmFloat :: Width -> LlvmType
widthToLlvmFloat W32  = LMFloat
widthToLlvmFloat W64  = LMDouble
widthToLlvmFloat W80  = LMFloat80
widthToLlvmFloat W128 = LMFloat128
widthToLlvmFloat w    = panic $ "widthToLlvmFloat: Bad float size: " ++ show w

-- | Translate a Cmm Bit Width to a LlvmType.
widthToLlvmInt :: Width -> LlvmType
widthToLlvmInt w = LMInt $ widthInBits w

-- | GHC Call Convention for LLVM
llvmGhcCC :: LlvmCallConvention
llvmGhcCC = CC_Ncc 10

-- | Llvm Function type for Cmm function
llvmFunTy :: LlvmType
llvmFunTy
  = LMFunction $
        LlvmFunctionDecl (fsLit "a") ExternallyVisible llvmGhcCC LMVoid FixedArgs
            (Left $ map getVarType llvmFunArgs) llvmFunAlign

-- | Llvm Function signature
llvmFunSig :: CLabel -> LlvmLinkageType -> LlvmFunctionDecl
llvmFunSig lbl link
  = let n = strCLabel_llvm lbl
    in LlvmFunctionDecl n link llvmGhcCC LMVoid FixedArgs
        (Right llvmFunArgs) llvmFunAlign

-- | Alignment to use for functions
llvmFunAlign :: LMAlign
llvmFunAlign = Just 4

-- | Alignment to use for into tables
llvmInfAlign :: LMAlign
llvmInfAlign = Just 4

-- | A Function's arguments
llvmFunArgs :: [LlvmVar]
llvmFunArgs = map lmGlobalRegArg activeStgRegs

-- | Llvm standard fun attributes
llvmStdFunAttrs :: [LlvmFuncAttr]
llvmStdFunAttrs = [NoUnwind]

-- | Pointer width
llvmPtrBits :: Int
llvmPtrBits = widthInBits $ typeWidth gcWord


-- ----------------------------------------------------------------------------
-- * Environment Handling
--

type LlvmEnvMap = UniqFM LlvmType
-- two maps, one for functions and one for local vars.
type LlvmEnv = (LlvmEnvMap, LlvmEnvMap)

-- | Get initial Llvm environment.
initLlvmEnv :: LlvmEnv
initLlvmEnv = (emptyUFM, emptyUFM)

-- | Clear variables from the environment.
clearVars :: LlvmEnv -> LlvmEnv
clearVars (e1, _) = (e1, emptyUFM)

-- | Insert functions into the environment.
varInsert, funInsert :: Uniquable key => key -> LlvmType -> LlvmEnv -> LlvmEnv
varInsert s t (e1, e2) = (e1, addToUFM e2 s t)
funInsert s t (e1, e2) = (addToUFM e1 s t, e2)

-- | Lookup functions in the environment.
varLookup, funLookup :: Uniquable key => key -> LlvmEnv -> Maybe LlvmType
varLookup s (_, e2) = lookupUFM e2 s
funLookup s (e1, _) = lookupUFM e1 s


-- ----------------------------------------------------------------------------
-- * Label handling
--

-- | Pretty print a 'CLabel'.
strCLabel_llvm :: CLabel -> LMString
strCLabel_llvm l = (fsLit . show . llvmSDoc . pprCLabel) l

-- | Create an external definition for a 'CLabel' defined in another module.
genCmmLabelRef :: CLabel -> LMGlobal
genCmmLabelRef = genStringLabelRef . strCLabel_llvm

-- | As above ('genCmmLabelRef') but taking a 'LMString', not 'CLabel'.
genStringLabelRef :: LMString -> LMGlobal
genStringLabelRef cl
  = let ty = LMPointer $ LMArray 0 llvmWord
    in (LMGlobalVar cl ty External Nothing Nothing, Nothing)


-- ----------------------------------------------------------------------------
-- * Misc
--

-- | Error function
panic :: String -> a
panic s = Outp.panic $ "LlvmCodeGen.Base." ++ s

