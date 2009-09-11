-- ----------------------------------------------------------------------------
-- Pretty print LLVM
--

module LlvmCodeGen.Ppr where

#include "HsVersions.h"

import Llvm
import Llvm.PpLlvm
import LlvmCodeGen.Base

import DynFlags
import Outputable ( panic, showSDocOneLine )
import qualified Outputable
import Pretty

-- ----------------------------------------------------------------------------
-- Top level
--

pprLlvmHeader :: Doc
pprLlvmHeader = ppLlvmGlobal mainCapability

pprLlvmCmmTop :: DynFlags -> LlvmCmmTop -> Doc
pprLlvmCmmTop _ _
  = Outputable.pprTrace "pprLlvmCmmTop not implemented!" Outputable.empty empty

pprLlvmData :: DynFlags -> LlvmData -> Doc
pprLlvmData _ (globals, alias, struct) =
    let globals' = ppLlvmGlobals globals
        alias'   = ppLlvmTypeAlias alias
        struct'  = ppLlvmGlobal struct
    in globals' $+$ alias' $+$ struct'

