-- ----------------------------------------------------------------------------
-- Base LLVM Code Generation module
--
-- Contains functions useful through out the code generator.
--

module LlvmCodeGen.Base (

        LlvmCmmTop, LlvmBlock, LlvmUnresData, LlvmData, UnresLabel, UnresStatic,
        LlvmEnv,
        getLlvmType, getFloatWidth, getBitWidth, llvmWord, llvmFunTy,
        stringInCStyle, genLlvmStr,
        mainCapability, strCLabel_llvm, genCmmLabelRef, genStringLabelRef,
        llvmSDoc

    ) where

#include "HsVersions.h"

import Llvm
import Llvm.PpLlvm

import CLabel
import Cmm
import CmmExpr
import DynFlags
import ErrUtils
import Outputable ( panic, showSDocOneLine )
import qualified Outputable
import Pretty

import Data.Char
import qualified Data.Map as Map
import Data.Word
import Numeric

-- ----------------------------------------------------------------------------
-- Some data types
--

type LlvmCmmTop = GenCmmTop CmmStatic [CmmStatic] (ListGraph LlvmStatement)
type LlvmBlock = GenBasicBlock LlvmStatement

type LlvmUnresData = (String, LlvmType, [UnresStatic])
type LlvmData = ([LMGlobal], LlvmType, LMGlobal)

type UnresLabel = CmmLit
type UnresStatic = Either UnresLabel LlvmStatic

type LlvmEnv = Map.Map String LlvmType

-- ----------------------------------------------------------------------------
-- Type translations
--

-- | Translate a basic CmmType to an LlvmType.
getLlvmType :: CmmType -> LlvmType
getLlvmType ty | isFloatType ty = getFloatWidth $ typeWidth ty
               | otherwise      = getBitWidth   $ typeWidth ty

-- | Translate a Cmm Float Width to a LlvmType.
getFloatWidth :: Width -> LlvmType
getFloatWidth W32  = LMFloat
getFloatWidth W64  = LMDouble
getFloatWidth W80  = LMFloat80
getFloatWidth W128 = LMFloat128
getFloatWidth _    = panic "LlvmCodeGen.Data.getFloatWidth - Invalid float size"

-- | Translate a Cmm Bit Width to a LlvmType.
getBitWidth :: Width -> LlvmType
getBitWidth w = LMInt $ widthInBits w

-- | Llvm word
llvmWord :: LlvmType
llvmWord = getBitWidth wordWidth

-- | Llvm Function type for Cmm function
llvmFunTy :: LlvmType
llvmFunTy = LMFunction i32 []


-- ----------------------------------------------------------------------------
-- String handling
--

-- | Print strings as valid C strings
stringInCStyle :: [Word8] -> String
stringInCStyle s = concatMap genLlvmStr s

genLlvmStr :: Word8 -> String
genLlvmStr w =
    let conv c | isLlvmOk c = [c]
                          | otherwise  = hex w
                
        isLlvmOk l = isAscii l && isPrint l

        hex w' = let s = showHex w' ""
                 in case s of
                     []       -> panic "Llvm.Base.genLlvmStr - returned nothing"
                     (x:[])   -> ['\\','0',(toUpper x)]
                     (x:y:[]) -> ['\\',(toUpper x),(toUpper y)]
                     _        -> panic "Llvm.Base.genLlvmStr - returned too much"

    in conv (chr $ fromIntegral w) 


-- ----------------------------------------------------------------------------
-- Label handling
--

-- | Add external reference to the MainCapability
mainCapability :: LMGlobal
mainCapability = genCmmLabelRef mkMainCapabilityLabel

-- | Pretty print a CLabel
strCLabel_llvm :: CLabel -> String
strCLabel_llvm l 
	= show $ llvmSDoc (pprCLabel l)

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

