-- -----------------------------------------------------------------------------
--
-- This is the top-level module in the llvm code generator.
--
-- -----------------------------------------------------------------------------

\begin{code}
{-# OPTIONS -w #-}

module LlvmCodeGen ( llvmCodeGen ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Base
import LlvmCodeGen.Data
import LlvmCodeGen.Ppr
import LlvmCodeGen.Regs

import CLabel
import Cmm
import CmmOpt ( cmmMiniInline, cmmMachOpFold )
import PprCmm

import BufWrite
import DynFlags
import ErrUtils
import FastString
import Outputable
import UniqSupply
import qualified Pretty as Prt

import qualified Data.Map as Map
import System.IO

-- -----------------------------------------------------------------------------
-- Top-level of the llvm codegen
--
llvmCodeGen :: DynFlags -> Handle -> UniqSupply -> [RawCmm] -> IO ()
llvmCodeGen dflags h us cmms
  = do
      putMsg dflags (text "llvmCodeGen: Not yet supported! will FAIL!")
      putMsg dflags (text $ "Please use the C (-fvia-C) or native code"
                 ++ " generation backend (-fasm)")

      let cmm = concat $ map extractRawCmm cmms
      let cmmProc = concat $ map extractCmmProc cmm

      bufh <- newBufHandle h

      Prt.bufLeftRender bufh $ pprLlvmHeader

      cmmDataLlvmGens dflags bufh cmm
      cmmProcLlvmGens dflags bufh us cmmProc 0

      bFlush bufh

      return  ()

  where extractRawCmm (Cmm tops) = tops
        extractCmmProc cp@(CmmProc _ _ _ _) = [cp]
        extractCmmProc _                    = []


-- -----------------------------------------------------------------------------
-- Do native code generation on all these cmms data sections.
--
cmmDataLlvmGens
      :: DynFlags
      -> BufHandle
      -> [RawCmmTop]
      -> IO ()

cmmDataLlvmGens dflags h cmm =
    let exData cd@(CmmData s d) = [(s,d)]
        exData _                = []
    in cmmDataLlvmGens' dflags h Map.empty (concat $ map exData cmm) []
      
cmmDataLlvmGens'
      :: DynFlags
      -> BufHandle
      -> LlvmEnv
      -> [(Section, [CmmStatic])]
      -> [LlvmUnresData]
      -> IO ()

cmmDataLlvmGens' dflags h env [] lmdata
    = do
        let lmdata' = map (resolveLlvmData dflags env) lmdata
        let lmdoc = Prt.vcat $ map (pprLlvmData dflags) lmdata'
        Prt.bufLeftRender h lmdoc
        return ()

cmmDataLlvmGens' dflags h env (cmm:cmms) lmdata
    = do
        let lmdata'@(l, ty, d)  = genLlvmData dflags cmm
        let env' = Map.insert l ty env
        cmmDataLlvmGens' dflags h env' cmms (lmdata ++ [lmdata'])


-- -----------------------------------------------------------------------------
-- Do native code generation on all these cmms procs.
--
cmmProcLlvmGens
      :: DynFlags
      -> BufHandle
      -> UniqSupply
      -> [RawCmmTop]
      -> Int
      -> IO ()

cmmProcLlvmGens dflags h us [] count
    = return ()

cmmProcLlvmGens dflags h us ((CmmData _ _) : cmms) count
    = panic "Llvm.LlvmCodeGen.cmmProcLlvmGens - CmmData section found!"

cmmProcLlvmGens dflags h us (cmm : cmms) count
  = do
      (us', llvm) <- cmmLlvmGen dflags us cmm count

      Prt.bufLeftRender h $
          Prt.vcat $ map (\x -> Prt.empty Prt.$+$ (pprLlvmCmmTop dflags x) Prt.$+$ Prt.empty) llvm

      let count' = count + 1

      cmmProcLlvmGens dflags h us' cmms count'


-- | Complete llvm code generation phase for a single top-level chunk of Cmm.
--
cmmLlvmGen
      :: DynFlags
      -> UniqSupply
      -> RawCmmTop                                    -- ^ the cmm to generate code for
      -> Int                                          -- ^ sequence number of this top thing
      -> IO ( UniqSupply
              , [LlvmCmmTop] )                          -- native code

cmmLlvmGen dflags us cmm count
  = do
    -- rewrite assignments to global regs
    let (fixed_cmm, usFix) = initUs us $ fixAssignsTop cmm

    -- cmm to cmm optimisations
    let opt_cmm = cmmToCmm dflags fixed_cmm

    dumpIfSet_dyn dflags Opt_D_dump_opt_cmm "LLVM Cmm Input"
        (pprCmm $ Cmm [opt_cmm])

    -- generate native code from cmm
    let (llvmBC, usGen) = initUs usFix $ genLlvmCode dflags opt_cmm

    return (usGen, llvmBC)


-- -----------------------------------------------------------------------------
-- Instruction selection
--

genLlvmCode 
    :: DynFlags 
    -> RawCmmTop 
    -> UniqSM [LlvmCmmTop]

genLlvmCode dflags (CmmData sec dat)
    = return [CmmData sec dat]

genLlvmCode dflags (CmmProc info lab params (ListGraph blocks))
    = return []


-- -----------------------------------------------------------------------------
-- Fixup assignments to global registers so that they assign to 
-- locations within the RegTable, if appropriate.

-- Note that we currently don't fixup reads here: they're done by
-- the generic optimiser below, to avoid having two separate passes
-- over the Cmm.
-- 
-- The LLVM back-end doesn't support pinned registers as of yet.
--

fixAssignsTop :: RawCmmTop -> UniqSM RawCmmTop

fixAssignsTop top@(CmmData _ _) = returnUs top

fixAssignsTop (CmmProc info lbl params (ListGraph blocks)) =
    mapUs fixAssignsBlock blocks `thenUs` \ blocks' ->
    returnUs (CmmProc info lbl params (ListGraph blocks'))

fixAssignsBlock :: CmmBasicBlock -> UniqSM CmmBasicBlock
fixAssignsBlock (BasicBlock id stmts) =
    fixAssigns stmts `thenUs` \ stmts' ->
    returnUs (BasicBlock id stmts')

fixAssigns :: [CmmStmt] -> UniqSM [CmmStmt]
fixAssigns stmts =
    mapUs fixAssign stmts `thenUs` \ stmtss ->
    returnUs (concat stmtss)

fixAssign :: CmmStmt -> UniqSM [CmmStmt]
fixAssign (CmmAssign (CmmGlobal reg) src)
  | Left expr <- reg_or_addr
  = pprPanic ("The LLVM Back-end doesn't support the use of real registers for"
        ++ " STG registers") (text "Use an unregistered build instead"
        $$+$$ (PprCmm.pprExpr  expr))
  | Right baseRegAddr <- reg_or_addr
  = returnUs [CmmStore baseRegAddr src]
          -- Replace register leaves with appropriate StixTrees for
          -- the given target. GlobalRegs which map to a reg on this
          -- arch are left unchanged.  Assigning to BaseReg is always
          -- illegal, so we check for that.
  where
      reg_or_addr = get_GlobalReg_addr reg

fixAssign other_stmt = returnUs [other_stmt]


-- -----------------------------------------------------------------------------
-- Generic Cmm optimiser
--

{-
Here we do:

  (a) Constant folding
  (b) Simple inlining: a temporary which is assigned to and then
      used, once, can be shorted.
  (c) Replacement of references to GlobalRegs which do not have
      machine registers by the appropriate memory load (eg.
      Hp ==>  *(BaseReg + 34) ).
-}

cmmToCmm :: DynFlags -> RawCmmTop -> RawCmmTop
cmmToCmm _ top@(CmmData _ _) = top
cmmToCmm dflags (CmmProc info lbl params (ListGraph blocks)) =
    let blocks' = map cmmBlockConFold (cmmMiniInline blocks)
    in CmmProc info lbl params (ListGraph blocks')

cmmBlockConFold :: CmmBasicBlock -> CmmBasicBlock
cmmBlockConFold (BasicBlock id stmts) =
    let stmts' = map cmmStmtConFold stmts
    in BasicBlock id stmts'

cmmStmtConFold :: CmmStmt -> CmmStmt
cmmStmtConFold stmt
    = case stmt of
        CmmAssign reg src
            -> let src' = cmmExprConFold src
               in case src' of
                      CmmReg reg' | reg == reg' -> CmmNop
                      new_src -> CmmAssign reg new_src

        CmmStore addr src
            -> let addr' = cmmExprConFold addr
                   src'  = cmmExprConFold src
               in CmmStore addr' src'

        CmmJump addr regs
            -> let addr' = cmmExprConFold addr
               in CmmJump addr' regs

        CmmCall target regs args srt returns
            -> let target' = case target of
                        CmmCallee e conv ->
                            let e' = cmmExprConFold e
                            in CmmCallee e' conv
                        other -> other
                   args' = map (\(CmmHinted arg hint) ->
                           let arg' = cmmExprConFold arg
                           in (CmmHinted arg' hint)) args
               in CmmCall target' regs args' srt returns

        CmmCondBranch test dest
            -> let test' = cmmExprConFold test
               in case test' of
                      CmmLit (CmmInt 0 _) -> 
                          CmmComment (mkFastString ("deleted: " ++ 
                              showSDoc (pprStmt stmt)))

                      CmmLit (CmmInt n _) -> CmmBranch dest

                      other -> CmmCondBranch test' dest

        CmmSwitch expr ids
            -> let expr' = cmmExprConFold expr
               in CmmSwitch expr' ids

        other
            -> other


cmmExprConFold :: CmmExpr -> CmmExpr
cmmExprConFold expr
    = case expr of
        CmmLoad addr rep
            -> let addr' = cmmExprConFold addr
               in CmmLoad addr' rep

        CmmMachOp mop args
            -- For MachOps, we first optimize the children, and then we try 
            -- our hand at some constant-folding.
             -> let args' = map (cmmExprConFold) args
                in cmmMachOpFold mop args'

        CmmReg (CmmGlobal mid)
            -- Replace register leaves with appropriate StixTrees for
            -- the given target.  MagicIds which map to a reg on this
            -- arch are left unchanged.  For the rest, BaseReg is taken
            -- to mean the address of the reg table in MainCapability,
            -- and for all others we generate an indirection to its
            -- location in the register table.
            -> case get_GlobalReg_addr mid of
                    Left  realreg -> pprPanic ("The LLVM Back-end doesn't " ++
                        "support the use of real registers for STG registers")
                        (text "Use an unregistered build instead" $$+$$
                        (PprCmm.pprExpr  expr))
                    Right baseRegAddr 
                        -> case mid of 
                            BaseReg -> cmmExprConFold baseRegAddr
                            other   -> cmmExprConFold 
                                           (CmmLoad baseRegAddr (globalRegType mid))

            -- eliminate zero offsets
        CmmRegOff reg 0
            -> cmmExprConFold (CmmReg reg)

        CmmRegOff (CmmGlobal mid) offset
            -- RegOf leaves are just a shorthand form. If the reg maps
            -- to a real reg, we keep the shorthand, otherwise, we just
            -- expand it and defer to the above code. 
            -> case get_GlobalReg_addr mid of
                    Left  realreg -> pprPanic ("The LLVM Back-end doesn't " ++
                        "support the use of real registers for STG registers")
                        (text "Use an unregistered build instead" $$+$$
                        (PprCmm.pprExpr  expr))
                    Right baseRegAddr
                        -> cmmExprConFold (CmmMachOp (MO_Add wordWidth) [
                                CmmReg (CmmGlobal mid),
                                CmmLit (CmmInt (fromIntegral offset)
                                    wordWidth)])

        other
            -> other


-- -----------------------------------------------------------------------------
-- Misc

-- | $+$ for SDoc with an blank line between
($$+$$) :: SDoc -> SDoc -> SDoc
p $$+$$ q = p $+$ empty $+$ q

\end{code}

