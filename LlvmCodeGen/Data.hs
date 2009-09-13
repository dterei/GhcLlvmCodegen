-- ----------------------------------------------------------------------------
-- Handle conversion of CmmData to LLVM code.
--

module LlvmCodeGen.Data (
        genLlvmData, resolveLlvmData
    ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Base

import BlockId
import CLabel
import Cmm
import CmmExpr
import DynFlags
import Outputable ( panic, showSDocOneLine )
import qualified Outputable
import Pretty

import qualified Data.Map as Map
import Data.Maybe


-- ----------------------------------------------------------------------------
-- Constants
--

structStr :: String
structStr = "_struct"

-- ----------------------------------------------------------------------------
-- Top level
--

-- | Pass a CmmStatic section to an equivalent Llvm code. Can't 
--   complete this completely though as we need to pass all CmmStatic
--   sections before all references can be resolved. This last step is
--   done by 'resolveLlvmData'.
genLlvmData :: DynFlags -> (Section, [CmmStatic]) -> LlvmUnresData
genLlvmData _ ( _ , (CmmDataLabel lbl):xs) =
    let static  = map genData xs
        label   = strCLabel_llvm lbl

        types   = map getStatTypes static
        getStatTypes (Left  x) = getLlvmType $ cmmLitType x
        getStatTypes (Right x) = getStatType x

        strucTy = LMStruct types
        alias   = LMAlias (label ++ structStr) strucTy
    in (label, alias, static)

genLlvmData _ _ = panic "Bad CmmData section doesn't start with CLabel!"


-- | Fix up CLabel references now that we should have passed all CmmData.
resolveLlvmData :: DynFlags -> LlvmEnv -> LlvmUnresData -> LlvmData
resolveLlvmData _ env (label, alias, unres) =
    let (static, refs) = unzip $ map (resData env) unres
        refs'          = catMaybes (concat refs)
        struct         = Just $ LMStaticStruc static alias
        glob           = LMGlobalVar label alias ExternallyVisible
    in (refs', alias, (glob, struct))


-- ----------------------------------------------------------------------------
-- Conversion functions
--
 
-- | Resolve an individual static label if it needs to be.
--   We check the 'LlvmEnv' to see if the reference has been defined in this
--   module. If it has we can retrieve its type and make a pointer, otherwise
--   we introduce a generic external defenition for the referenced label and
--   then make a pointer.
resData :: LlvmEnv -> UnresStatic -> (LlvmStatic, [Maybe LMGlobal])

resData env (Right stat) = (stat, [Nothing])

resData env (Left cmm@(CmmLabel l)) =
    let label = strCLabel_llvm l
        ty = Map.lookup label env
        lmty = getLlvmType $ cmmLitType cmm
    in case ty of
            -- Make generic external label defenition and then pointer to it
            Nothing -> 
                let glob@(var, _) = genStringLabelRef label
                    ptr  = LMStaticPointer var
                in  (LMPtoI ptr lmty, [Just glob])
            -- Referenced data exists in this module, retrieve type and make
            -- pointer to it.
            Just ty' ->
                let var = LMGlobalVar label (LMPointer ty') ExternallyVisible
                    ptr  = LMStaticPointer var
                in (LMPtoI ptr lmty, [Nothing])

resData env (Left (CmmLabelOff label off)) =
    let (var, glob) = resData env (Left (CmmLabel label))
        offset = LMStaticLit $ LMIntLit (toInteger off) llvmWord
    in (LMAdd var offset, glob)

-- FIX: Check this actually works
resData env (Left (CmmLabelDiffOff l1 l2 off)) =
    let (var1, glob1) = resData env (Left (CmmLabel l1))
        (var2, glob2) = resData env (Left (CmmLabel l2))
        var = LMSub var1 var2
        offset = LMStaticLit $ LMIntLit (toInteger off) llvmWord
    in (LMAdd var offset, glob1 ++ glob2)


-- | Handle static data
--   Don't handle CmmAlign or a CmmDataLabel.
genData :: CmmStatic -> UnresStatic

genData (CmmString str)
    = Right $ LMString (stringInCStyle str) (LMArray (1 + length str) i8)

genData (CmmUninitialised bytes)
    = Right $ LMUninitType (LMArray bytes i8)

genData (CmmStaticLit lit)
    = genStaticLit lit

genData (CmmAlign bytes)
    = panic "Llvm.Base.genData - Can't handle CmmAlign!"

genData (CmmDataLabel lbl)
    = panic "Llvm.Base.genData - Can't handle data labels not at top of data!"


-- | Pretty print a static literal.
--   Returns a tuple, the first element being the static value, the second
--   value being a supporting forward reference if needed (e.g for labels).
genStaticLit :: CmmLit -> UnresStatic
genStaticLit (CmmInt i w)
    = Right $ LMStaticLit (LMIntLit i (LMInt $ widthInBits w))

genStaticLit (CmmFloat r w)
    = Right $ LMStaticLit (LMFloatLit r (getFloatWidth w))

-- Leave unresolved, will fix later
genStaticLit c@(CmmLabel        _    ) = Left $ c
genStaticLit c@(CmmLabelOff     _   _) = Left $ c
genStaticLit c@(CmmLabelDiffOff _ _ _) = Left $ c

-- FIX: Check this actually works
genStaticLit (CmmBlock b) = Left $ CmmLabel $ infoTblLbl b

genStaticLit (CmmHighStackMark)
    = panic "LlvmCodeGen.Data.genStaticLit - CmmHighStackMark unsupported!"
  
