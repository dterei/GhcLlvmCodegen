-- ----------------------------------------------------------------------------
-- Handle conversion of CmmProc to LLVM code.
--
module LlvmCodeGen.CodeGen where

import Llvm
import LlvmCodeGen.Base

import BlockId
import CLabel
import Cmm

import FastString ( unpackFS )
import Outputable ( panic )
import UniqSupply
import Unique

import qualified Data.Map as Map

-- -----------------------------------------------------------------------------
-- Top-level of the llvm proc codegen
--
genLlvmProc :: LlvmEnv -> RawCmmTop -> UniqSM (LlvmEnv, [LlvmCmmTop])
genLlvmProc env (CmmData _ _)
  = return (env, [])

genLlvmProc env (CmmProc _ _ _ (ListGraph []))
  = return (env, [])

genLlvmProc env (CmmProc info lbl params (ListGraph blocks))
  = do
        (env', lmblocks, lmdata) <- basicBlocksCodeGen env blocks ([], [])

        let proc 	= CmmProc info lbl params (ListGraph lmblocks)
        let tops 	= lmdata ++ [proc]

        return (env', tops)

-- -----------------------------------------------------------------------------
-- Block code generation
--

-- | Generate code for a list of blocks. Should all be in same procedure.
basicBlocksCodeGen :: LlvmEnv
                   -> [CmmBasicBlock]
                   -> ( [LlvmBasicBlock] , [LlvmCmmTop] )
                   -> UniqSM (LlvmEnv, [LlvmBasicBlock] , [LlvmCmmTop] )
basicBlocksCodeGen env ([]) (blocks, tops)
  = return (env, blocks, tops)

basicBlocksCodeGen env (block:blocks) (lblocks', ltops')
  = do (env', lb, lt) <- basicBlockCodeGen env block
       let lblocks = lblocks' ++ lb
       let ltops   = ltops' ++ lt
       basicBlocksCodeGen env' blocks (lblocks, ltops)


-- | Generate code for one block
basicBlockCodeGen ::  LlvmEnv
                  -> CmmBasicBlock
                  -> UniqSM ( LlvmEnv, [LlvmBasicBlock], [LlvmCmmTop] )
basicBlockCodeGen env (BasicBlock id stmts)
  = do (env', instrs, top) <- stmtsToInstrs env stmts ([], [])
       return (env', [(BasicBlock id instrs)], top)


-- | Convert a list of CmmStmt's to LlvmStatement's
stmtsToInstrs :: LlvmEnv
              -> [CmmStmt]
              -> ([LlvmStatement], [LlvmCmmTop])
              -> UniqSM (LlvmEnv, [LlvmStatement], [LlvmCmmTop])
stmtsToInstrs env [] (llvm, top)
  = return (env, llvm, top)

stmtsToInstrs env (stmt : stmts) (llvm, top)
   = do (env', instrs, tops) <- stmtToInstrs env stmt
        stmtsToInstrs env' stmts (llvm ++ instrs, top ++ tops)


-- -----------------------------------------------------------------------------
-- CmmStmt code generation
--
  
-- | Convert a CmmStmt to a list of LlvmStatement's
stmtToInstrs :: LlvmEnv
             -> CmmStmt
             -> UniqSM (LlvmEnv, [LlvmStatement], [LlvmCmmTop])
stmtToInstrs env stmt = case stmt of

    CmmNop
        -> return (env, [], [])

    CmmComment s
        -> return (env, [Comment [(unpackFS s)]], [])

    CmmAssign reg src
        | isFloatType ty -> return (env, [], [])
        | otherwise      -> return (env, [], [])
        where ty = cmmRegType reg

    CmmStore addr src
        | isFloatType ty -> return (env, [], [])
        | otherwise      -> return (env, [], [])
        where ty = cmmExprType src

    CmmCall target result_regs args _ _
        -> return (env, [], [])

    CmmBranch id
        -> let label =  LMLocalVar (blockIdToLlvm id) LMLabel
           in return (env, [Branch label], [])

    CmmCondBranch arg id
        -> return (env, [], [])

    CmmSwitch arg ids
        -> return (env, [], [])

    CmmJump arg _
        -> return (env, [], [])

    CmmReturn _       
        -> panic $ "LlvmCodeGen.CodeGen.stmtToInstrs: return statement should"
                ++ "have been cps'd away"


-- -----------------------------------------------------------------------------
-- CmmExpr code generation
--

-- | Convert a CmmExpr to a list of LlvmStatements with the result of the
--   expression being stored in the returned LlvmVar. Even for simple code
--   such as literals we take this approach and rely on the LLVM optimiser
--   to fix up the inefficiency of the code.
exprToVar :: LlvmEnv
          -> CmmExpr
          -> UniqSM (LlvmEnv, LlvmVar, [LlvmStatement], [LlvmCmmTop])
exprToVar env e = 
    let v' = LMLocalVar "nothing" i32
    in case e of

        CmmLit lit
            -> genLit env lit 

        CmmLoad e' ty
            -> genCmmLoad env e' ty

        CmmReg r
            -> return $ genCmmReg env r
        
        CmmMachOp op exprs
            -> genCmmMachop op exprs

        CmmStackSlot _ _ 
            -> panic "LlvmCodeGen.CodeGen.exprToVar: CmmStackSlot not supported!"
        
        CmmRegOff r i
            -> exprToVar env $ expandCmmReg (r, i)


-- | Handle CmmMachOp expressions
genMachOp :: LlvmEnv -> MachOp -> [CmmExpr]
          -> UniqSM (LlvmEnv, LlvmVar, [LlvmStatement], [LlvmCmmTop])
genMachOp env op exprs = return (env, LMLocalVar "nothing" i32, [], [])


-- | Handle CmmLoad expression
genCmmLoad :: LlvmEnv -> CmmExpr -> CmmType
           -> UniqSM (LlvmEnv, LlvmVar, [LlvmStatement], [LlvmCmmTop])
genCmmLoad env e ty = do
    (env', iptr, stmts, tops) <- exprToVar env e
    let ety = getVarType iptr
    case (isInt ety) of
         True | llvmPtrBits == llvmWidthInBits ety ->  do
                    let pty = LMPointer $ getLlvmType ty
                    ptr <- mkLocalVar pty
                    let cast = Assignment ptr (Cast iptr pty)
                    dvar <- mkLocalVar $ getLlvmType ty
                    let load = Assignment dvar (Load ptr)
                    return (env', dvar, stmts ++ [cast, load], tops)

              | otherwise
                -> panic $ "LlvmCodeGen.CodeGen.exprToVar: can't cast"
                        ++ " to pointer as int not of pointer size!"

         False -> panic $ "LlvmCodeGen.CodeGen.exprToVar: CmmLoad expression"
                        ++ " is not of type int!"


-- | Handle CmmReg expression
genCmmReg :: LlvmEnv -> CmmReg 
          -> (LlvmEnv, LlvmVar, [LlvmStatement], [LlvmCmmTop])
genCmmReg env r@(CmmLocal (LocalReg un ty))
  = let name = uniqToStr un
        oty  = Map.lookup name env

        (newv, stmts) = allocReg r
        -- FIX: Should remove from env or put in proc only env. This env is
        -- global to module and shouldn't contain local vars. Could also strip
        -- local vars from env at end of proc generation.
        nenv = Map.insert name (getVarType newv) env
    in case oty of
            Just ety -> (env, (LMLocalVar name ety), [], [])
            Nothing  -> (nenv, newv, stmts, [])

genCmmReg _ _ = panic $ "LlvmCodeGen.CodeGen.genCmmReg: Global reg encountered!"
                    ++ " Registered build not supported!"


-- | Allocate a CmmReg on the stack
allocReg :: CmmReg -> (LlvmVar, [LlvmStatement])
allocReg (CmmLocal (LocalReg un ty))
  = let ty' = getLlvmType ty
        var = LMLocalVar (uniqToStr un) (LMPointer ty')
        alc = Alloca ty' 1
    in (var, [Assignment var alc])

allocReg _ = panic $ "LlvmCodeGen.CodeGen.allocReg: Global reg encountered!"
                    ++ " Registered build not supported!"


-- | Generate code for a literal
genLit :: LlvmEnv -> CmmLit
       -> UniqSM (LlvmEnv, LlvmVar, [LlvmStatement], [LlvmCmmTop])

genLit env (CmmInt i w)
  = return (env, LMLitVar $ LMIntLit i (LMInt $ widthInBits w), [], [])

genLit env (CmmFloat r w)
  = return (env, LMLitVar $ LMFloatLit r (getFloatWidth w), [], [])


genLit env cmm@(CmmLabel l)
  = let label = strCLabel_llvm l
        ty = Map.lookup label env
        lmty = getLlvmType $ cmmLitType cmm
    in case ty of
            -- Make generic external label defenition and then pointer to it
            Nothing -> do 
                let glob@(var, _) = genStringLabelRef label
                let ldata = [CmmData Data [([glob], [])]]
                let env' = Map.insert label (pLower $ getVarType var) env
                tmp1 <- mkLocalVar llvmWord
                let stm1 = Assignment tmp1 (Cast var llvmWord)
                return (env', tmp1, [stm1], ldata)
            -- Referenced data exists in this module, retrieve type and make
            -- pointer to it.
            Just ty' -> do
                let var = LMGlobalVar label (LMPointer ty') ExternallyVisible
                tmp1 <- mkLocalVar llvmWord
                let stm1 = Assignment tmp1 (Cast var llvmWord)
                return (env, tmp1, [stm1], [])

genLit env (CmmLabelOff label off) = do
    (env', vlbl, stmts, stat) <- genLit env (CmmLabel label)
    let voff = LMLitVar $ LMIntLit (toInteger off) llvmWord
    tmp1 <- mkLocalVar (getVarType vlbl)
    let stmt1 = Assignment tmp1 (MachOp LM_MO_Add vlbl voff)
    return (env', tmp1, stmts ++ [stmt1], stat)

genLit env (CmmLabelDiffOff l1 l2 off) = do
    (env1, vl1, stmts1, stat1) <- genLit env (CmmLabel l1)
    (env2, vl2, stmts2, stat2) <- genLit env1 (CmmLabel l2)
    let voff = LMLitVar $ LMIntLit (toInteger off) llvmWord
    let ty1 = getVarType vl1
    let ty2 = getVarType vl2
    if (isInt ty1) && (isInt ty2)
       && (llvmWidthInBits ty1 == llvmWidthInBits ty2)

       then do
            tmp1 <- mkLocalVar (getVarType vl1)
            let stmt1 = Assignment tmp1 (MachOp LM_MO_Sub vl1 vl2)
            tmp2 <- mkLocalVar (getVarType tmp1)
            let stmt2 = Assignment tmp2 (MachOp LM_MO_Add tmp1 voff)
            return (env2, tmp2, stmts1 ++ stmts2 ++ [stmt1, stmt2], stat1 ++ stat2)

        else
            panic $ "LlvmCodeGen.CodeGen.genLit: CmmLabelDiffOff encountered"
                    ++ " with different label types!"

genLit env (CmmBlock b)
  = genLit env (CmmLabel $ infoTblLbl b)

genLit env (CmmHighStackMark)
  = panic "LlvmCodeGen.Data.genStaticLit - CmmHighStackMark unsupported!"
  

-- -----------------------------------------------------------------------------
-- Misc
--

-- | Create a new local var
mkLocalVar :: LlvmType -> UniqSM LlvmVar
mkLocalVar ty = do
    us <- getUniqueUs
    return $ LMLocalVar (uniqToStr us) ty


-- Expand CmmRegOff
expandCmmReg :: (CmmReg, Int) -> CmmExpr
expandCmmReg (reg, off)
	= CmmMachOp (MO_Add width) [CmmReg reg, CmmLit (CmmInt (fromIntegral off) width)]
	where width = typeWidth (cmmRegType reg)


-- | Convert a block id into a appropriate string for LLVM use.
blockIdToLlvm :: BlockId -> String
blockIdToLlvm (BlockId id) = uniqToStr id


-- | Convert a Unique to a corresponding string representation.
uniqToStr :: Unique -> String
uniqToStr u = strCLabel_llvm $ mkAsmTempLabel u

