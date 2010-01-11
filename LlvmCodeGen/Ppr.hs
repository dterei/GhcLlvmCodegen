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

-- ----------------------------------------------------------------------------
-- Top level
--

-- | Header code for LLVM modules
-- FIX: This is i386 (x86) hardcoded at moment.
pprLlvmHeader :: Doc
pprLlvmHeader
  =  (text "target datalayout = \"e-p:32:32:32-i1:8:8-i8:8:8-i16:16:16-i32:32:32-i64:32:64-f32:32:32-f64:32:64-v64:64:64-v128:128:128-a0:0:64-f80:32:32\"")
 $+$ (text "target triple = \"i386-linux-gnu\"")
 $+$ (ppLlvmGlobal mainCapability)


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
            fun = LlvmFunction funDec [NoUnwind] lmblocks
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
        (_, ldata) = resolveLlvmData dflags initLlvmEnv unres
    in pprLlvmData dflags ldata

