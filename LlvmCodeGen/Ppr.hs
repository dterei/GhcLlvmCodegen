-- ----------------------------------------------------------------------------
-- Pretty print LLVM
--

module LlvmCodeGen.Ppr (
        pprLlvmHeader, pprLlvmCmmTop, pprLlvmData
    ) where

#include "HsVersions.h"

import Llvm
import Llvm.PpLlvm
import LlvmCodeGen.Base
import LlvmCodeGen.Data

import CLabel
import Cmm
import DynFlags
import Outputable ( panic, showSDocOneLine )
import qualified Outputable
import Pretty

import qualified Data.Map as Map

-- ----------------------------------------------------------------------------
-- Top level
--

-- | Header code for LLVM modules
pprLlvmHeader :: Doc
pprLlvmHeader = ppLlvmGlobal mainCapability


-- | Pretty print LLVM code
pprLlvmCmmTop :: DynFlags -> LlvmCmmTop -> Doc
pprLlvmCmmTop dflags (CmmData _ lmdata)
  = vcat $ map (pprLlvmData dflags) lmdata

pprLlvmCmmTop dflags p@(CmmProc info lbl params (ListGraph stmts))
  = (
        let static   = CmmDataLabel (entryLblToInfoLbl lbl) : info
        in if not (null info)
            then pprCmmStatic dflags static
            else empty
    ) $+$ (
        let link = if (externallyVisibleCLabel lbl)
                        then ExternallyVisible else Internal
            funDec = llvmFunSig lbl link
            blocks = [LlvmBlock "entry" [Return (LMLitVar $ LMIntLit 0 llvmWord)]]
            fun = LlvmFunction funDec [NoUnwind] blocks
        in ppLlvmFunction fun
    )

pprLlvmCmmTop _ _
  = Outputable.pprTrace "pprLlvmCmmTop not implemented!" Outputable.empty empty


-- | Pretty print LLVM data code
pprLlvmData :: DynFlags -> LlvmData -> Doc
pprLlvmData _ (globals, alias ) =
    let globals' = ppLlvmGlobals globals
        alias'   = ppLlvmTypeAliases alias
    in alias' $+$ globals'

-- | Pretty print CmmStatic
pprCmmStatic :: DynFlags -> [CmmStatic] -> Doc
pprCmmStatic dflags stat
  = let unres = genLlvmData dflags (Data,stat)
        (env', ldata) = resolveLlvmData dflags Map.empty unres
    in pprLlvmData dflags ldata

