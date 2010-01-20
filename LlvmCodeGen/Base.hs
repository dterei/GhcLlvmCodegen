-- ----------------------------------------------------------------------------
-- Base LLVM Code Generation module
--
-- Contains functions useful through out the code generator.
--

module LlvmCodeGen.Base (

        LlvmCmmTop, LlvmBasicBlock,
        LlvmUnresData, LlvmData, UnresLabel, UnresStatic,

        LlvmEnv, initLlvmEnv, clearVars, varLookup, varInsert,
        funLookup, funInsert,

        cmmToLlvmType, widthToLlvmFloat, widthToLlvmInt, llvmFunTy,
        llvmFunSig, llvmStdFunAttrs, llvmPtrBits,

        strBlockId_llvm, strCLabel_llvm,
        genCmmLabelRef, genStringLabelRef, llvmSDoc

    ) where

#include "HsVersions.h"

import Llvm
#ifndef NO_REGS
import LlvmCodeGen.Regs
#endif

import BlockId
import CLabel
import Cmm

import Outputable ( ppr )
import qualified Outputable
import Pretty
import Unique

import qualified Data.Map as Map

-- ----------------------------------------------------------------------------
-- Some data types
--

type LlvmCmmTop = GenCmmTop LlvmData [CmmStatic] (ListGraph LlvmStatement)
type LlvmBasicBlock = GenBasicBlock LlvmStatement

-- (data label, data type, unresovled data)
type LlvmUnresData = (CLabel, LlvmType, [UnresStatic])

-- (data, type aliases)
type LlvmData = ([LMGlobal], [LlvmType])

type UnresLabel = CmmLit
type UnresStatic = Either UnresLabel LlvmStatic

-- ----------------------------------------------------------------------------
-- Type translations
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
widthToLlvmFloat w    = panic $ "widthToLlvmFloat: Invalid float size, " ++ show w

-- | Translate a Cmm Bit Width to a LlvmType.
widthToLlvmInt :: Width -> LlvmType
widthToLlvmInt w = LMInt $ widthInBits w

-- | Llvm Function type for Cmm function
llvmFunTy :: LlvmType
llvmFunTy
  = LMFunction $
        LlvmFunctionDecl "a" ExternallyVisible CC_Fastcc LMVoid FixedArgs
#ifndef NO_REGS
            (Left [llvmWord, llvmWord, llvmWord, llvmWord])
#else /* Unregistered build */
            (Left [])
#endif

-- | Llvm Function signature
llvmFunSig :: CLabel -> LlvmLinkageType -> LlvmFunctionDecl
llvmFunSig lbl link
  = let n = strCLabel_llvm lbl
    in LlvmFunctionDecl n link CC_Fastcc LMVoid FixedArgs
#ifndef NO_REGS
        (Right [lmBaseArg, lmSpArg, lmHpArg, lmR1Arg])
#else /* Unregistered build */
        (Left [])
#endif

-- | Llvm standard fun attributes
llvmStdFunAttrs :: [LlvmFuncAttr]
llvmStdFunAttrs = [NoUnwind]

-- | Pointer width
llvmPtrBits :: Int
llvmPtrBits = widthInBits $ typeWidth gcWord


-- ----------------------------------------------------------------------------
-- Enviornment Handling
--

type LlvmEnvMap = Map.Map LMString LlvmType
-- two maps, one for functions and one for local vars.
type LlvmEnv = (LlvmEnvMap, LlvmEnvMap)

-- | Get initial LlvmEnv.
initLlvmEnv :: LlvmEnv
initLlvmEnv = (Map.empty, Map.empty)

-- | clear vars
clearVars :: LlvmEnv -> LlvmEnv
clearVars (e1, _) = (e1, Map.empty)

-- | insert functions
varInsert, funInsert :: LMString -> LlvmType -> LlvmEnv -> LlvmEnv
varInsert s t (e1, e2) = (e1, Map.insert s t e2)
funInsert s t (e1, e2) = (Map.insert s t e1, e2)

-- | lookup functions
varLookup, funLookup :: LMString -> LlvmEnv -> Maybe LlvmType
varLookup s (_, e2) = Map.lookup s e2
funLookup s (e1, _) = Map.lookup s e1


-- ----------------------------------------------------------------------------
-- Label handling
--

-- | Pretty Print a BlockId
strBlockId_llvm :: BlockId -> LMString
strBlockId_llvm b = (show . llvmSDoc . ppr . getUnique) b

-- | Pretty print a CLabel
strCLabel_llvm :: CLabel -> LMString
strCLabel_llvm l = show $ llvmSDoc (pprCLabel l)

-- | Create an external defenition for a CLabel that is defined in another module.
genCmmLabelRef :: CLabel -> LMGlobal
genCmmLabelRef cl =
    let mcl = strCLabel_llvm cl
    in (LMGlobalVar mcl (LMPointer (LMArray 0 llvmWord)) External, Nothing)

-- | As above ('genCmmLabelRef') but taking a LMString, not CLabel.
genStringLabelRef :: LMString -> LMGlobal
genStringLabelRef cl =
    (LMGlobalVar cl (LMPointer (LMArray 0 llvmWord)) External, Nothing)


-- ----------------------------------------------------------------------------
-- Misc
--

-- | Convert SDoc to Doc
llvmSDoc :: Outputable.SDoc -> Doc
llvmSDoc d
	= Outputable.withPprStyleDoc (Outputable.mkCodeStyle Outputable.CStyle) d

-- | error function
panic :: String -> a
panic s = Outputable.panic $ "LlvmCodeGen.Base." ++ s

