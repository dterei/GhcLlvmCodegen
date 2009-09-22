-- ----------------------------------------------------------------------------
-- Base LLVM Code Generation module
--
-- Contains functions useful through out the code generator.
--

module LlvmCodeGen.Base (

        LlvmCmmTop, LlvmBasicBlock, LlvmUnresData, LlvmData, UnresLabel,
        UnresStatic, LlvmEnv,

        cmmToLlvmType, widthToLlvmFloat, widthToLlvmInt, llvmWord, llvmFunTy,
        llvmFunSig, llvmPtrBits, genLlvmStr,

        initLlvmEnv, mainCapability, strBlockId_llvm, strCLabel_llvm,
        genCmmLabelRef, genStringLabelRef, llvmSDoc

    ) where

#include "HsVersions.h"

import Llvm

import BlockId
import CLabel
import Cmm
import CmmExpr
import DynFlags
import ErrUtils
import Outputable ( ppr )
import qualified Outputable
import Pretty
import Unique

import Data.Char
import qualified Data.Map as Map
import Data.Word
import Numeric

-- ----------------------------------------------------------------------------
-- Some data types
--

type LlvmCmmTop = GenCmmTop LlvmData [CmmStatic] (ListGraph LlvmStatement)
type LlvmBasicBlock = GenBasicBlock LlvmStatement

type LlvmUnresData = (CLabel, LlvmType, [UnresStatic])

-- (data, type aliases)
type LlvmData = ([LMGlobal], [LlvmType])

type UnresLabel = CmmLit
type UnresStatic = Either UnresLabel LlvmStatic

type LlvmEnv = Map.Map String LlvmType

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

-- | Llvm word
llvmWord :: LlvmType
llvmWord = widthToLlvmInt wordWidth

-- | Llvm Function type for Cmm function
llvmFunTy :: LlvmType
llvmFunTy
  = LMFunction $
        LlvmFunctionDecl "a" ExternallyVisible CC_Fastcc LMVoid FixedArgs []

-- | Llvm Function signature
llvmFunSig :: CLabel -> LlvmLinkageType -> LlvmFunctionDecl
llvmFunSig lbl link
  = let n = strCLabel_llvm lbl
    in LlvmFunctionDecl n link CC_Fastcc LMVoid FixedArgs []

-- | Pointer width
llvmPtrBits :: Int
llvmPtrBits = widthInBits $ typeWidth gcWord

-- | Get initial LlvmEnv.
initLlvmEnv :: LlvmEnv
initLlvmEnv
  = let n = getPlainName $ getGlobalVar mainCapability
        t = pLower $ getGlobalType mainCapability
    in Map.insert n t Map.empty


-- ----------------------------------------------------------------------------
-- String handling
--

-- | Print strings as valid C strings
genLlvmStr :: [Word8] -> String
genLlvmStr s = concatMap genLlvmStr' s

genLlvmStr' :: Word8 -> String
genLlvmStr' w =
    let conv c | isLlvmOk c = [c]
               | otherwise  = hex w
                
        isLlvmOk l = isAscii l && isPrint l && l /= '"'

        hex l = let s = showHex l ""
                in case s of
                    []       -> panic "genLlvmStr': returned nothing"
                    (x:[])   -> ['\\','0',(toUpper x)]
                    (x:y:[]) -> ['\\',(toUpper x),(toUpper y)]
                    _        -> panic "genLlvmStr': returned too much"

    in conv (chr $ fromIntegral w) 


-- ----------------------------------------------------------------------------
-- Label handling
--

-- | Add external reference to the MainCapability
mainCapability :: LMGlobal
mainCapability = genCmmLabelRef mkMainCapabilityLabel

-- | Pretty Print a BlockId
strBlockId_llvm :: BlockId -> String
strBlockId_llvm b = (show . llvmSDoc . ppr . getUnique) b

-- | Pretty print a CLabel
strCLabel_llvm :: CLabel -> String
strCLabel_llvm l = show $ llvmSDoc (pprCLabel l)

-- | Create an external defenition for a CLabel that is defined in another module.
genCmmLabelRef :: CLabel -> LMGlobal
genCmmLabelRef cl =
    let mcl = strCLabel_llvm cl
    in (LMGlobalVar mcl (LMPointer (LMArray 0 llvmWord)) External, Nothing)

-- | As above ('genCmmLabelRef') but taking a String, not CLabel.
genStringLabelRef :: String -> LMGlobal
genStringLabelRef cl =
    (LMGlobalVar cl (LMPointer (LMArray 0 llvmWord)) External, Nothing)


-- ----------------------------------------------------------------------------
-- Misc
--

-- | Convert SDoc to Doc
llvmSDoc :: Outputable.SDoc -> Doc
llvmSDoc d 
	= Outputable.withPprStyleDoc (Outputable.mkCodeStyle Outputable.AsmStyle) d

-- | error function
panic :: String -> a
panic s = Outputable.panic $ "LlvmCodeGen.Base." ++ s

