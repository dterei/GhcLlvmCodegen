-- ----------------------------------------------------------------------------
-- Pretty print LLVM
--

module LlvmCodeGen.Ppr (
        pprLlvmHeader, pprLlvmCmmTop, pprLlvmData
    ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Base
import LlvmCodeGen.Data

import CLabel
import Cmm

import DynFlags
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

pprLlvmCmmTop dflags (CmmProc info lbl _ (ListGraph blocks))
  = (
        let static = CmmDataLabel (entryLblToInfoLbl lbl) : info
        in if not (null info)
            then pprCmmStatic dflags static
            else empty
    ) $+$ (
        let link = if (externallyVisibleCLabel lbl)
                        then ExternallyVisible else Internal
            funDec = llvmFunSig lbl link
            lmblocks = map (\(BasicBlock id stmts) -> LlvmBlock (strBlockId_llvm id) stmts) blocks
            fun = LlvmFunction funDec [NoUnwind, NoReturn] lmblocks
        in ppLlvmFunction fun
    )


-- | Pretty print LLVM data code
pprLlvmData :: DynFlags -> LlvmData -> Doc
pprLlvmData _ (globals, types ) =
    let globals' = ppLlvmGlobals globals
        types'   = ppLlvmTypes types
    in types' $+$ globals'


-- | Pretty print CmmStatic
pprCmmStatic :: DynFlags -> [CmmStatic] -> Doc
pprCmmStatic dflags stat
  = let unres = genLlvmData dflags (Data,stat)
        (_, ldata) = resolveLlvmData dflags Map.empty unres
    in pprLlvmData dflags ldata

