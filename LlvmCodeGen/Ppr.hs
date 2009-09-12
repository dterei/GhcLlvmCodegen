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
pprLlvmCmmTop dflags cmm@(CmmData sec stat)
  = let unres = genLlvmData dflags (sec,stat)
        ldata = resolveLlvmData dflags Map.empty unres
    in pprLlvmData dflags ldata

pprLlvmCmmTop dflags p@(CmmProc info lbl params (ListGraph stmts))
  = (
        let static   = CmmDataLabel (entryLblToInfoLbl lbl) : info
            infoData = CmmData Data static
        in if not (null info)
            then pprLlvmCmmTop dflags infoData
            else empty
    ) $+$ (
        let funDec = LlvmFunctionDecl (strCLabel_llvm lbl) i32 FixedArgs []
            link = if (externallyVisibleCLabel lbl)
                        then ExternallyVisible else Internal
            blocks = [LlvmBasicBlock "entry" [Return (LMLitVar $ LMIntLit 0 llvmWord)]]
            fun = LlvmFunction funDec link [NoUnwind] blocks
        in ppLlvmFunction fun
    )

pprLlvmCmmTop _ _
  = Outputable.pprTrace "pprLlvmCmmTop not implemented!" Outputable.empty empty


-- | Pretty print LLVM data code
pprLlvmData :: DynFlags -> LlvmData -> Doc
pprLlvmData _ (globals, alias, struct) =
    let globals' = ppLlvmGlobals globals
        alias'   = ppLlvmTypeAlias alias
        struct'  = ppLlvmGlobal struct
    in globals' $+$ alias' $+$ struct'

