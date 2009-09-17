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

-- -----------------------------------------------------------------------------
-- CmmStmt code generation
--

type StmtData = (LlvmEnv, [LlvmStatement], [LlvmCmmTop])

-- | Convert a list of CmmStmt's to LlvmStatement's
stmtsToInstrs :: LlvmEnv -> [CmmStmt] -> ([LlvmStatement], [LlvmCmmTop])
              -> UniqSM StmtData
stmtsToInstrs env [] (llvm, top)
  = return (env, llvm, top)

stmtsToInstrs env (stmt : stmts) (llvm, top)
   = do (env', instrs, tops) <- stmtToInstrs env stmt
        stmtsToInstrs env' stmts (llvm ++ instrs, top ++ tops)

  
-- | Convert a CmmStmt to a list of LlvmStatement's
--
stmtToInstrs :: LlvmEnv -> CmmStmt
             -> UniqSM StmtData
stmtToInstrs env stmt = case stmt of

    CmmNop -> return (env, [], [])
    CmmComment s -> return (env, [Comment [(unpackFS s)]], [])

    CmmAssign reg src    -> genAssign env reg src
    CmmStore addr src    -> genStore env addr src

    CmmBranch id         -> genBranch env id
    CmmCondBranch arg id -> genCondBranch env arg id
    CmmSwitch arg ids    -> genSwitch env arg ids

    -- Foreign Call
    -- TODO: Implement
    CmmCall target result_regs args _ _
        -> return (env, [], [])

    -- Tail call
    CmmJump arg _ -> genJump env arg

    -- CPS, only tail calls, no return's
    CmmReturn _       
        -> panic $ "LlvmCodeGen.CodeGen.stmtToInstrs: return statement should"
                ++ "have been cps'd away"


-- | Tail function calls
--
genJump :: LlvmEnv -> CmmExpr -> UniqSM StmtData

-- Call to known function
genJump env (CmmLit (CmmLabel lbl)) = do
    (env', vf, stmts, top) <- getFunc env lbl
    let s1 = Expr $ Call TailCall vf []
    return (env', stmts ++ [s1], top)

-- Call to unknown function / address
genJump env expr = do
    let fty = llvmFunTy
    (env', vf, stmts, top) <- exprToVar env expr

    let cast = case getVarType vf of
         ty | isPointer ty -> LM_Bitcast
         ty | isInt ty     -> LM_Inttoptr

         ty -> panic $ "LlvmCodeGen.CodeGen.genJump: Expr is of bad type for"
                    ++ " function call! (" ++ show (ty) ++ ")"

    v1 <- mkLocalVar fty
    let s1 = Assignment v1 (Cast cast vf fty)
    let s2 = Expr $ Call TailCall v1 []
    return (env', stmts ++ [s1,s2], top)



-- | CmmAssign operation
--
-- We use stack allocated variables for CmmReg. The optimiser will replace
-- these with registers when possible.
genAssign :: LlvmEnv -> CmmReg -> CmmExpr -> UniqSM StmtData
genAssign _ (CmmGlobal _) _
 = panic $ "The LLVM Back-end doesn't support the use of real registers for"
        ++ " STG registers. Use an unregistered build instead. They should"
        ++ " have been removed earlier in the code generation!"

genAssign env reg val = do
    (env1, vreg, stmts1, top1) <- exprToVar env (CmmReg reg)
    (env2, vval, stmts2, top2) <- exprToVar env1 val
    let s1 = Store vval vreg
    return (env2, stmts1 ++ stmts2 ++ [s1], top1 ++ top2)


-- | CmmStore operation
genStore :: LlvmEnv -> CmmExpr -> CmmExpr -> UniqSM StmtData
genStore env addr val = do
    (env1, vaddr, stmts1, top1) <- exprToVar env addr
    (env2, vval,  stmts2, top2) <- exprToVar env1 val
    if getVarType vaddr == llvmWord
        then do
            let vty = pLift $ getVarType vval
            vptr <- mkLocalVar vty
            let s1 = Assignment vptr (Cast LM_Inttoptr vaddr vty)
            let s2 = Store vval vptr
            return (env2, stmts1 ++ stmts2 ++ [s1, s2], top1 ++ top2)

        else
            panic $ "LlvmCodeGen.CodeGen.genStore: ptr not of word size!"


-- | Unconditional branch
genBranch :: LlvmEnv -> BlockId -> UniqSM StmtData
genBranch env id =
    let label = blockIdToLlvm id
    in return (env, [Branch label], [])
 

-- | Conditional branch
genCondBranch :: LlvmEnv -> CmmExpr -> BlockId -> UniqSM StmtData
genCondBranch env cond idT = do
    idF <- mkUniqStr
    let labelT = blockIdToLlvm idT
    let labelF = LMLocalVar idF LMLabel
    (env', vc, stmts, top) <- exprToVar env cond
    if getVarType vc == i1
        then do
            let stmt1 = BranchIf vc labelT labelF
            let stmt2 = MkLabel idF
            return $ (env', stmts ++ [stmt1, stmt2], top)
        else
            panic $ "LlvmCodeGen.CodeGen.genCondBranch: Cond expr not bool!"


-- | Switch branch
--
--   N.B. we remove Nothing's from the list of branches, as they are
--   'undefined'. However, they may be defined one day, so we better document
--   this behaviour.
genSwitch :: LlvmEnv -> CmmExpr -> [Maybe BlockId] -> UniqSM StmtData
genSwitch env cond maybe_ids = do
    (env', vc, stmts, top) <- exprToVar env cond
    let ty = getVarType vc

    let pairs = [ (ix, id) | (ix,Just id) <- zip ([0..]::[Integer]) maybe_ids ]
    let labels = map (\(ix, b) -> (mkIntLit ix ty, blockIdToLlvm b)) pairs
    -- out of range is undefied, so lets just branch to first label
    -- FIX: Maybe create new error branch instead?
    let (_, defLbl) = head labels

    let stmt1 = Switch vc defLbl labels
    return $ (env', stmts ++ [stmt1], top)


-- -----------------------------------------------------------------------------
-- CmmExpr code generation
--

type ExprData = (LlvmEnv, LlvmVar, LlvmStatements, [LlvmCmmTop])

-- | Convert a CmmExpr to a list of LlvmStatements with the result of the
--   expression being stored in the returned LlvmVar. Even for simple code
--   such as literals we take this approach and rely on the LLVM optimiser
--   to fix up the inefficiency of the code.
exprToVar :: LlvmEnv -> CmmExpr -> UniqSM ExprData
exprToVar env e = case e of

    CmmLit lit
        -> genLit env lit 

    CmmLoad e' ty
        -> genCmmLoad env e' ty

    CmmReg r
        -> return $ genCmmReg env r
    
    CmmMachOp op exprs
        -> genMachOp env op exprs
    
    CmmRegOff r i
        -> exprToVar env $ expandCmmReg (r, i)

    CmmStackSlot _ _ 
        -> panic "LlvmCodeGen.CodeGen.exprToVar: CmmStackSlot not supported!"


-- | Handle CmmMachOp expressions
--
genMachOp :: LlvmEnv -> MachOp -> [CmmExpr] -> UniqSM ExprData

-- | Unary Machop
genMachOp env op [x] = case op of

    MO_Not w -> 
        let all1 = mkIntLit (-1) (getBitWidth w)
        in negate (getBitWidth w) all1 LM_MO_Xor

    MO_S_Neg w ->
        let all0 = mkIntLit 0 (getBitWidth w)
        in negate (getBitWidth w) all0 LM_MO_Sub

    MO_F_Neg w ->
        let all0 = LMLitVar $ LMFloatLit 0 (getFloatWidth w)
        in negate (getFloatWidth w) all0 LM_MO_FSub

    MO_SF_Conv _ w -> fiConv (getFloatWidth w) LM_Sitofp
    MO_FS_Conv _ w -> fiConv (getBitWidth w) LM_Fptosi

    MO_SS_Conv from to
        -> sameConv from (getBitWidth to) LM_Trunc LM_Sext

    MO_UU_Conv from to
        -> sameConv from (getBitWidth to) LM_Trunc LM_Zext

    MO_FF_Conv from to
        -> sameConv from (getFloatWidth to) LM_Fptrunc LM_Fpext

    _ -> panic "LlvmCodeGen.CodeGen.genMachOp: unmatched unary CmmMachOp!"

    where
        negate ty v2 negOp = do
            (env', vx, stmts, top) <- exprToVar env x
            tmp1 <- mkLocalVar ty
            let stmt1 = Assignment tmp1 (LlvmOp negOp vx v2)
            return (env', tmp1, stmts ++ [stmt1], top)

        fiConv ty convOp = do
            (env', vx, stmts, top) <- exprToVar env x
            tmp1 <- mkLocalVar ty
            let stmt1 = Assignment tmp1 (Cast convOp vx ty)
            return (env', tmp1, stmts ++ [stmt1], top)

        sameConv from ty reduce expand = do
            (env', vx, stmts, top) <- exprToVar env x
            tmp1 <- mkLocalVar ty
            if widthInBits from > llvmWidthInBits ty
                then do -- reduce
                    let stmt1 = Assignment tmp1 (Cast reduce vx ty)
                    return (env', tmp1, stmts ++ [stmt1], top)
                else do -- expand
                    let stmt1 = Assignment tmp1 (Cast expand vx ty)
                    return (env', tmp1, stmts ++ [stmt1], top)


-- | Binary MachOp
genMachOp env op [x, y] = case op of

    MO_Eq _   -> genBinComp LM_CMP_Eq
    MO_Ne _   -> genBinComp LM_CMP_Ne

    MO_S_Gt _ -> genBinComp LM_CMP_Sgt
    MO_S_Ge _ -> genBinComp LM_CMP_Sge
    MO_S_Lt _ -> genBinComp LM_CMP_Slt
    MO_S_Le _ -> genBinComp LM_CMP_Sle

    MO_U_Gt _ -> genBinComp LM_CMP_Ugt
    MO_U_Ge _ -> genBinComp LM_CMP_Uge
    MO_U_Lt _ -> genBinComp LM_CMP_Ult
    MO_U_Le _ -> genBinComp LM_CMP_Ule

    MO_Add _ -> genBinMach LM_MO_Add
    MO_Sub _ -> genBinMach LM_MO_Sub
    MO_Mul _ -> genBinMach LM_MO_Mul

    MO_U_MulMayOflo w
        -> panic "LlvmCodeGen.CodeGen.genMachOp: MO_U_MulMayOflo unsupported!"

    MO_S_MulMayOflo w -> isSMulOK w x y

    MO_S_Quot _ -> genBinMach LM_MO_SDiv
    MO_S_Rem  _ -> genBinMach LM_MO_SRem

    MO_U_Quot _ -> genBinMach LM_MO_UDiv
    MO_U_Rem  _ -> genBinMach LM_MO_URem

    MO_F_Eq _ -> genBinComp LM_CMP_Feq
    MO_F_Ne _ -> genBinComp LM_CMP_Fne
    MO_F_Gt _ -> genBinComp LM_CMP_Fgt
    MO_F_Ge _ -> genBinComp LM_CMP_Fge
    MO_F_Lt _ -> genBinComp LM_CMP_Flt
    MO_F_Le _ -> genBinComp LM_CMP_Fle

    MO_F_Add  _ -> genBinMach LM_MO_FAdd
    MO_F_Sub  _ -> genBinMach LM_MO_FSub
    MO_F_Mul  _ -> genBinMach LM_MO_FMul
    MO_F_Quot _ -> genBinMach LM_MO_FDiv

    MO_And _   -> genBinMach LM_MO_And
    MO_Or  _   -> genBinMach LM_MO_Or
    MO_Xor _   -> genBinMach LM_MO_Xor
    MO_Shl _   -> genBinMach LM_MO_Shl
    MO_U_Shr _ -> genBinMach LM_MO_LShr
    MO_S_Shr _ -> genBinMach LM_MO_AShr

    _ -> panic $ "LlvmCodeGen.CodeGen.genMachOp: unmatched binary CmmMachOp!"

    where
        binLlvmOp ty binOp = do
            (env1, vx, stmts1, top1) <- exprToVar env x
            (env2, vy, stmts2, top2) <- exprToVar env1 y
            if getVarType vx == getVarType vy
                then do
                    tmp1 <- mkLocalVar $ ty vx
                    let stmt1 = Assignment tmp1 (binOp vx vy)
                    return (env2, tmp1, stmts1 ++ stmts2 ++ [stmt1], top1 ++ top2)
                else
                    panic $ "LlvmCodeGen.CodeGen.genMachOp: comparison between"
                        ++ " different types!"

        genBinComp cmp = binLlvmOp (\x -> i1) $ Compare cmp
        
        genBinMach op = binLlvmOp getVarType (LlvmOp op)

        -- | Detect if overflow will occur in signed multiply of the two
        --   CmmExpr's. This is the LLVM assembly equivalent of the NCG
        --   implementation. Its much longer due to type information/safety.
        isSMulOK :: Width -> CmmExpr -> CmmExpr -> UniqSM ExprData
        isSMulOK w x y = do
            (env1, vx, stmts1, top1) <- exprToVar env x
            (env2, vy, stmts2, top2) <- exprToVar env1 y

            let word  = getVarType vx
            let word2 = LMInt $ 2 * (llvmWidthInBits $ getVarType vx)
            let shift = llvmWidthInBits word
            let shift1 = mkIntLit (shift - 1) llvmWord
            let shift2 = mkIntLit shift llvmWord

            if isInt word
                then do
                    x1 <- mkLocalVar word2
                    y1 <- mkLocalVar word2
                    let s1 = Assignment x1 (Cast LM_Sext vx word2)
                    let s2 = Assignment y1 (Cast LM_Sext vy word2)
                    r1 <- mkLocalVar word2
                    let s3 = Assignment r1 (LlvmOp LM_MO_Mul x1 y1)
                    rlow1 <- mkLocalVar word
                    let s4 = Assignment rlow1 (Cast LM_Trunc r1 word)
                    rlow2 <- mkLocalVar word
                    let s5 = Assignment rlow2 (LlvmOp LM_MO_AShr rlow1 shift1)
                    rhigh1 <- mkLocalVar word2
                    let s6 = Assignment rhigh1 (LlvmOp LM_MO_AShr r1 shift2)
                    rhigh2 <- mkLocalVar word
                    let s7 = Assignment rhigh2 (Cast LM_Trunc rhigh1 word)
                    dst <- mkLocalVar word
                    let s8 = Assignment dst (LlvmOp LM_MO_Sub rlow2 rhigh2)

                    return (env2, dst, stmts1 ++ stmts2 ++
                            [s1, s2, s3, s4, s5, s6, s7, s8], top1 ++ top2)

                else
                    panic $ "LlvmCodeGen.CodeGen.isSMulOK: Not bit type!"


-- | More then two expression, invalid!
genMachOp _ _ _ = panic $ "LlvmCodeGen.CodeGen.genMachOp: More then 2"
                        ++ " expressions in MachOp!"


-- | Handle CmmLoad expression
genCmmLoad :: LlvmEnv -> CmmExpr -> CmmType -> UniqSM ExprData
genCmmLoad env e ty = do
    (env', iptr, stmts, tops) <- exprToVar env e
    let ety = getVarType iptr
    case (isInt ety) of
         True | llvmPtrBits == llvmWidthInBits ety ->  do
                    let pty = LMPointer $ getLlvmType ty
                    ptr <- mkLocalVar pty
                    let cast = Assignment ptr (Cast LM_Inttoptr iptr pty)
                    dvar <- mkLocalVar $ getLlvmType ty
                    let load = Assignment dvar (Load ptr)
                    return (env', dvar, stmts ++ [cast, load], tops)

              | otherwise
                -> panic $ "LlvmCodeGen.CodeGen.exprToVar: can't cast"
                        ++ " to pointer as int not of pointer size!"

         False -> panic $ "LlvmCodeGen.CodeGen.exprToVar: CmmLoad expression"
                        ++ " is not of type int!"


-- | Handle CmmReg expression
genCmmReg :: LlvmEnv -> CmmReg -> ExprData
genCmmReg env r@(CmmLocal (LocalReg un ty))
  = let name = uniqToStr un
        oty  = Map.lookup name env

        (newv, stmts) = allocReg r
        -- FIX: Should remove from env or put in proc only env. This env is
        -- global to module and shouldn't contain local vars. Could also strip
        -- local vars from env at end of proc generation.
        nenv = Map.insert name (pLower $ getVarType newv) env
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
genLit :: LlvmEnv -> CmmLit -> UniqSM ExprData
genLit env (CmmInt i w)
  = return (env, mkIntLit i (LMInt $ widthInBits w), [], [])

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
                let stm1 = Assignment tmp1 (Cast LM_Ptrtoint var llvmWord)
                return (env', tmp1, [stm1], ldata)
            -- Referenced data exists in this module, retrieve type and make
            -- pointer to it.
            Just ty' -> do
                let var = LMGlobalVar label (LMPointer ty') ExternallyVisible
                tmp1 <- mkLocalVar llvmWord
                let stm1 = Assignment tmp1 (Cast LM_Ptrtoint var llvmWord)
                return (env, tmp1, [stm1], [])

genLit env (CmmLabelOff label off) = do
    (env', vlbl, stmts, stat) <- genLit env (CmmLabel label)
    let voff = mkIntLit off llvmWord
    tmp1 <- mkLocalVar (getVarType vlbl)
    let stmt1 = Assignment tmp1 (LlvmOp LM_MO_Add vlbl voff)
    return (env', tmp1, stmts ++ [stmt1], stat)

genLit env (CmmLabelDiffOff l1 l2 off) = do
    (env1, vl1, stmts1, stat1) <- genLit env (CmmLabel l1)
    (env2, vl2, stmts2, stat2) <- genLit env1 (CmmLabel l2)
    let voff = mkIntLit off llvmWord
    let ty1 = getVarType vl1
    let ty2 = getVarType vl2
    if (isInt ty1) && (isInt ty2)
       && (llvmWidthInBits ty1 == llvmWidthInBits ty2)

       then do
            tmp1 <- mkLocalVar (getVarType vl1)
            let stmt1 = Assignment tmp1 (LlvmOp LM_MO_Sub vl1 vl2)
            tmp2 <- mkLocalVar (getVarType tmp1)
            let stmt2 = Assignment tmp2 (LlvmOp LM_MO_Add tmp1 voff)
            return (env2, tmp2,
                    stmts1 ++ stmts2 ++ [stmt1, stmt2],
                    stat1 ++ stat2)

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

getFunc :: LlvmEnv -> CLabel -> UniqSM ExprData
getFunc env lbl
  = let ty  = Map.lookup fn env
        fn  = strCLabel_llvm lbl
        def =  llvmFunSig lbl ExternallyVisible
        fty = LMFunction def

    in case ty of
        -- | Function in module in right form
        Just ty'@(LMFunction sig) -> do
            let fun = LMGlobalVar fn ty' (funcLinkage sig)
            return (env, fun, [], [])

        -- | label in module but not function pointer, convert
        Just ty' -> do
            let fun = LMGlobalVar fn ty' ExternallyVisible
            v1 <- mkLocalVar fty
            let s1 = Assignment v1 (Cast LM_Bitcast fun fty)
            return (env, v1, [s1], [])

        -- | label not in module, create external reference
        Nothing  -> do
            let fun = LMGlobalVar fn fty ExternallyVisible
            let top = CmmData Data [([],[fty])]
            return (env, fun, [], [top])


-- | Create a new local var
mkLocalVar :: LlvmType -> UniqSM LlvmVar
mkLocalVar ty = do
    str <- mkUniqStr
    return $ LMLocalVar str ty


-- Expand CmmRegOff
expandCmmReg :: (CmmReg, Int) -> CmmExpr
expandCmmReg (reg, off)
  = let width = typeWidth (cmmRegType reg)
        voff  = CmmLit $ CmmInt (fromIntegral off) width
    in CmmMachOp (MO_Add width) [CmmReg reg, voff]


-- | Convert a block id into a appropriate string for LLVM use.
blockIdToLlvm :: BlockId -> LlvmVar
blockIdToLlvm (BlockId id) = LMLocalVar (uniqToStr id) LMLabel


-- | Create a new uniq string
mkUniqStr :: UniqSM String
mkUniqStr = do
    us <- getUniqueUs
    return $ uniqToStr us


-- | Convert a Unique to a corresponding string representation.
uniqToStr :: Unique -> String
uniqToStr u = strCLabel_llvm $ mkAsmTempLabel u


-- | Create Llvm int Literal
mkIntLit :: Integral a => a -> LlvmType -> LlvmVar
mkIntLit i ty = LMLitVar $ LMIntLit (toInteger i) ty

