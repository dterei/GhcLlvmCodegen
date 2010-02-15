-- -----------------------------------------------------------------------------
--
-- This is the top-level module in the LLVM code generator.
--

\begin{code}

module LlvmCodeGen ( llvmCodeGen ) where

#include "HsVersions.h"

import LlvmCodeGen.Base
import LlvmCodeGen.CodeGen
import LlvmCodeGen.Data
import LlvmCodeGen.Ppr
import LlvmCodeGen.Regs

import Cmm
import CmmOpt ( cmmMiniInline, cmmMachOpFold )
import PprCmm

import BufWrite
import DynFlags
import ErrUtils
import FastString
import Outputable
import qualified Pretty as Prt
import UniqSupply

import System.IO

-- -----------------------------------------------------------------------------
-- Top-level of the llvm codegen
--
llvmCodeGen :: DynFlags -> Handle -> UniqSupply -> [RawCmm] -> IO ()
llvmCodeGen dflags h us cmms
  = do
      let cmm = concat $ map extractRawCmm cmms

      bufh <- newBufHandle h

      Prt.bufLeftRender bufh $ pprLlvmHeader

      env <- cmmDataLlvmGens dflags bufh cmm
      cmmProcLlvmGens dflags bufh us env cmm

      bFlush bufh

      return  ()

  where extractRawCmm (Cmm tops) = tops


-- -----------------------------------------------------------------------------
-- Do native code generation on all these cmms data sections.
--
cmmDataLlvmGens
      :: DynFlags
      -> BufHandle
      -> [RawCmmTop]
      -> IO ( LlvmEnv )

cmmDataLlvmGens _ _ []
  = return ( initLlvmEnv )

cmmDataLlvmGens dflags h cmm =
    let exData (CmmData s d) = [(s,d)]
        exData  _            = []

        exProclbl (CmmProc _ l _ _) = [(strCLabel_llvm l)]
        exProclbl  _                = []

        cdata = concat $ map exData cmm
        -- put the functions into the enviornment
        cproc = concat $ map exProclbl cmm
        env = foldl (\e l -> funInsert l llvmFunTy e) initLlvmEnv cproc
    in cmmDataLlvmGens' dflags h env cdata []

cmmDataLlvmGens'
      :: DynFlags
      -> BufHandle
      -> LlvmEnv
      -> [(Section, [CmmStatic])]
      -> [LlvmUnresData]
      -> IO ( LlvmEnv )

cmmDataLlvmGens' dflags h env [] lmdata
    = do
        let (env', lmdata') = resolveLlvmDatas dflags env lmdata []
        let lmdoc = Prt.vcat $ map (pprLlvmData dflags) lmdata'

        dumpIfSet_dyn dflags Opt_D_dump_llvm "LLVM Code" $ docToSDoc lmdoc

        Prt.bufLeftRender h lmdoc
        return env'

cmmDataLlvmGens' dflags h env (cmm:cmms) lmdata
    = do
        let lmdata'@(l, ty, _) = genLlvmData dflags cmm
        let env' = funInsert (strCLabel_llvm l) ty env
        cmmDataLlvmGens' dflags h env' cmms (lmdata ++ [lmdata'])


-- -----------------------------------------------------------------------------
-- Do native code generation on all these cmms procs.
--
cmmProcLlvmGens
      :: DynFlags
      -> BufHandle
      -> UniqSupply
      -> LlvmEnv
      -> [RawCmmTop]
      -> IO ()

cmmProcLlvmGens _ _ _ _ []
    = return ()

cmmProcLlvmGens dflags h us env (cmm : cmms)
  = do
      (us', env', llvm) <- cmmLlvmGen dflags us (clearVars env) cmm

      Prt.bufLeftRender h $ Prt.vcat $ map (pprLlvmCmmTop dflags) llvm

      cmmProcLlvmGens dflags h us' env' cmms


-- | Complete llvm code generation phase for a single top-level chunk of Cmm.
--
cmmLlvmGen
      :: DynFlags
      -> UniqSupply
      -> LlvmEnv
      -> RawCmmTop                                    -- ^ the cmm to generate code for
      -> IO ( UniqSupply,
              LlvmEnv,
              [LlvmCmmTop] )                          -- native code

cmmLlvmGen dflags us env cmm
  = do
    -- rewrite assignments to global regs
    let (fixed_cmm, usFix) = initUs us $ fixAssignsTop cmm

    -- cmm to cmm optimisations
    let opt_cmm = cmmToCmm fixed_cmm

    dumpIfSet_dyn dflags Opt_D_dump_opt_cmm "Optimised Cmm"
        (pprCmm $ Cmm [opt_cmm])

    -- generate native code from cmm
    let ((env', llvmBC), usGen) = initUs usFix $ genLlvmCode dflags env opt_cmm

    dumpIfSet_dyn dflags Opt_D_dump_llvm "LLVM Code"
        (vcat $ map (docToSDoc . pprLlvmCmmTop dflags) llvmBC)

    return (usGen, env', llvmBC)


-- -----------------------------------------------------------------------------
-- Instruction selection
--

genLlvmCode
    :: DynFlags
    -> LlvmEnv
    -> RawCmmTop
    -> UniqSM (LlvmEnv, [LlvmCmmTop])

genLlvmCode _ env (CmmData _ _)
    = return (env, [])

genLlvmCode _ env (CmmProc _ _ _ (ListGraph []))
    = return (env, [])

genLlvmCode _ env cp@(CmmProc _ _ _ _)
    = genLlvmProc env cp

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
fixAssign e@(CmmAssign (CmmGlobal reg) src)
  | Left _realreg <- reg_or_addr
  = returnUs [e]
  | Right baseRegAddr <- reg_or_addr
  = returnUs [CmmStore baseRegAddr src]
          -- Replace register leaves with appropriate StixTrees for
          -- the given target. GlobalRegs which map to a reg on this
          -- arch are left unchanged.  Assigning to BaseReg is always
          -- illegal, so we check for that.
  where
      reg_or_addr = getGlobalRegAddr reg

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

cmmToCmm :: RawCmmTop -> RawCmmTop
cmmToCmm top@(CmmData _ _) = top
cmmToCmm (CmmProc info lbl params (ListGraph blocks)) =
    let blocks' = map cmmBlockConFold (cmmMiniInline blocks)
        front   = take (length blocks' - 1) blocks'
        end     = cmmAddReturn $ last blocks'
    in CmmProc info lbl params (ListGraph $ front ++ [end])

cmmAddReturn :: CmmBasicBlock -> CmmBasicBlock
cmmAddReturn blk@(BasicBlock id stmts) =
    let front = take (length stmts - 1) stmts
        end   = last stmts
    in case end of
        CmmNop -- strip out nop and check again
            -> cmmAddReturn (BasicBlock id front)

        CmmComment _ -- strip out comment and check again
            -> cmmAddReturn (BasicBlock id front)

        CmmCall _ _ _ _ _
            -> blk

        CmmBranch _
            -> blk

        CmmCondBranch _ _
            -> blk

        CmmSwitch _ _
            -> blk

        CmmJump _ _
            -> blk

        CmmReturn _
            -> blk

        _other
            -> (BasicBlock id $ stmts ++ [(CmmReturn [])])

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

                      CmmLit (CmmInt _ _) -> CmmBranch dest

                      _other -> CmmCondBranch test' dest

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
            -> case getGlobalRegAddr mid of
                    Left  _realreg -> expr
                    Right baseRegAddr
                        -> case mid of
                            BaseReg -> cmmExprConFold baseRegAddr
                            _other  -> cmmExprConFold
                                           (CmmLoad baseRegAddr (globalRegType mid))

            -- eliminate zero offsets
        CmmRegOff reg 0
            -> cmmExprConFold (CmmReg reg)

        CmmRegOff (CmmGlobal mid) offset
            -- RegOf leaves are just a shorthand form. If the reg maps
            -- to a real reg, we keep the shorthand, otherwise, we just
            -- expand it and defer to the above code.
            -> case getGlobalRegAddr mid of
                    Left  _realreg -> expr
                    Right _baseRegAddr
                        -> cmmExprConFold (CmmMachOp (MO_Add wordWidth) [
                                CmmReg (CmmGlobal mid),
                                CmmLit (CmmInt (fromIntegral offset)
                                    wordWidth)])

        other
            -> other


\end{code}

