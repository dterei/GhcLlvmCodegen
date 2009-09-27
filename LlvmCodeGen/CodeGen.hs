-- ----------------------------------------------------------------------------
-- Handle conversion of CmmProc to LLVM code.
--

module LlvmCodeGen.CodeGen ( genLlvmProc ) where

#include "HsVersions.h"

import Llvm
import LlvmCodeGen.Base

import BlockId
import CLabel
import Cmm
import qualified PprCmm

import BasicTypes
import FastString
import ForeignCall
import Outputable ( ppr )
import qualified Outputable
import UniqSupply
import Unique
import Util

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

-- | Generate code for a list of blocks that make up a complete procedure.
basicBlocksCodeGen :: LlvmEnv
                   -> [CmmBasicBlock]
                   -> ( [LlvmBasicBlock] , [LlvmCmmTop] )
                   -> UniqSM (LlvmEnv, [LlvmBasicBlock] , [LlvmCmmTop] )
basicBlocksCodeGen env ([]) (blocks, tops)
  = do let (blocks', allocs) = mapAndUnzip dominateAllocs blocks
       let allocs' = concat allocs
       let ((BasicBlock id fstmts):rblocks) = blocks'
       let fblocks = (BasicBlock id (allocs' ++ fstmts)):rblocks
       return (env, fblocks, tops)

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


-- | Allocations need to be extracted so they can be moved to the entry
-- of a function to make sure they dominate all posible paths in the CFG.
dominateAllocs :: LlvmBasicBlock -> (LlvmBasicBlock, [LlvmStatement])
dominateAllocs (BasicBlock id stmts)
  = (BasicBlock id allstmts, allallocs)
    where
        (allstmts, allallocs) = foldl split ([],[]) stmts
        split (stmts', allocs) s@(Assignment _ (Alloca _ _))
            = (stmts', allocs ++ [s])
        split (stmts', allocs) other
            = (stmts' ++ [other], allocs)


-- -----------------------------------------------------------------------------
-- CmmStmt code generation
--

-- A statement conversion retrun.
--   * LlvmEnv: The new enviornment
--   * LlvmStatement: The compiled llvm statements.
--   * LlvmCmmTop: Any global data needed.
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

    CmmNop               -> return (env, [], [])
    CmmComment _         -> return (env, [], []) -- nuke comments
--  CmmComment s         -> return (env, [Comment (lines $ unpackFS s)], [])

    CmmAssign reg src    -> genAssign env reg src
    CmmStore addr src    -> genStore env addr src

    CmmBranch id         -> genBranch env id
    CmmCondBranch arg id -> genCondBranch env arg id
    CmmSwitch arg ids    -> genSwitch env arg ids

    -- Foreign Call
    CmmCall target res args _ ret
        -> genCall env target res args ret

    -- Tail call
    CmmJump arg _ -> genJump env arg

    -- CPS, only tail calls, no return's
    CmmReturn _
        -> panic "stmtToInstrs: return statement should have been cps'd away"


-- | Foreign Calls
--
genCall :: LlvmEnv -> CmmCallTarget -> HintedCmmFormals -> HintedCmmActuals -> CmmReturnInfo
        -> UniqSM StmtData

-- Write barrier needs to be handled specially as it is implemented as an llvm
-- intrinsic function.
genCall env (CmmPrim MO_WriteBarrier) _ _ _ = do
    let fname = "llvm.memory.barrier"
    let funSig =
            LlvmFunctionDecl
                fname
                ExternallyVisible
                CC_Ccc
                LMVoid
                FixedArgs
                [i1, i1, i1, i1, i1]
    let fty = LMFunction funSig

    let fv   = LMGlobalVar fname fty (funcLinkage funSig)
    let tops = case Map.lookup fname env of
                    Just _  -> []
                    Nothing -> [CmmData Data [([],[fty])]]

    let args = [lmTrue, lmTrue, lmTrue, lmTrue, lmTrue]
    let s1 = Expr $ Call StdCall fv args
    let env' = Map.insert fname fty env

    return (env', [s1], tops)

    where
        lmTrue :: LlvmVar
        lmTrue  = LMLitVar $ LMIntLit (-1) i1

-- Handle all other foreign calls and prim ops.
genCall env target res args ret = do

    -- paramater types
    let arg_type (CmmHinted _ AddrHint) = pLift i8
        -- cast pointers to i8*. Llvm equivalent of void*
        arg_type (CmmHinted expr _    ) = cmmToLlvmType $ cmmExprType expr

    -- ret type
    let ret_type ([]) = LMVoid
        ret_type ([CmmHinted _ AddrHint]) = pLift i8
        ret_type ([CmmHinted reg _])        = cmmToLlvmType $ localRegType reg
        ret_type t = panic $ "genCall: Too many return values! Can only handle"
                        ++ " 0 or 1, given " ++ show (length t) ++ "."

    -- tail call?
    let callType CmmMayReturn    = StdCall
        callType CmmNeverReturns = TailCall

    -- extract call convention
    let conv = case target of
            CmmCallee _ conv -> conv
            CmmPrim   _      -> PrimCallConv

    -- get llvm call convention
    let lmconv = case conv of
            CCallConv    -> CC_Ccc
#if i386_TARGET_ARCH || x86_64_TARGET_ARCH
            StdCallConv  -> CC_X86_Stdcc
#else
            StdCallConv  -> CC_Ccc
#endif
            CmmCallConv  -> CC_Fastcc
            PrimCallConv -> CC_Ccc

    -- fun types
    let ccTy  = callType ret
    let argTy = map arg_type args
    let retTy = ret_type res
    let funTy name = LMFunction $
            LlvmFunctionDecl name ExternallyVisible lmconv retTy FixedArgs argTy

    -- get paramter values
    (env1, argVars, stmts1, top1) <- arg_vars env args ([], [], [])

    -- get the return register
    let ret_reg ([CmmHinted reg hint]) = (reg, hint)
        ret_reg t = panic $ "genCall: Bad number of registers! Can only handle"
                        ++ " 1, given " ++ show (length t) ++ "."

	-- deal with call types
    let getFunPtr :: CmmCallTarget -> UniqSM ExprData
        getFunPtr targ = case targ of
            CmmCallee (CmmLit (CmmLabel lbl)) _ -> do
                let name = strCLabel_llvm lbl
                case Map.lookup name env1 of
                    Just ty'@(LMFunction sig) -> do
                        -- Function in module in right form
                        let fun = LMGlobalVar name ty' (funcLinkage sig)
                        return (env1, fun, [], [])

                    Just _ -> do
                        -- label in module but not function pointer, convert
                        let fty@(LMFunction sig) = funTy name
                        let fun = LMGlobalVar name fty (funcLinkage sig)
                        (v1, s1) <- doExpr (pLift fty)
                                        $ Cast LM_Bitcast fun (pLift fty)
                        return  (env1, v1, [s1], [])

                    Nothing -> do
                        -- label not in module, create external reference
                        let fty@(LMFunction sig) = funTy name
                        let fun = LMGlobalVar name fty (funcLinkage sig)
                        let top = CmmData Data [([],[fty])]
                        let env' = Map.insert name fty env1
                        return (env', fun, [], [top])

            CmmCallee expr _ -> do
                (env', v1, stmts, top) <- exprToVar env1 expr
                let fty = funTy "dynamic"
                let cast = case getVarType v1 of
                     ty | isPointer ty -> LM_Bitcast
                     ty | isInt ty     -> LM_Inttoptr

                     ty -> panic $ "genCall: Expr is of bad type for function"
                                ++ " call! (" ++ show (ty) ++ ")"

                (v2,s1) <- doExpr (pLift fty) $ Cast cast v1 (pLift fty)
                return (env', v2, stmts ++ [s1], top)

            CmmPrim mop -> do
                let name = cmmPrimOpFunctions mop
                let lbl  = mkForeignLabel name Nothing True IsFunction
                getFunPtr $ CmmCallee (CmmLit (CmmLabel lbl)) CCallConv

    (env2, fptr, stmts2, top2) <- getFunPtr target

    let retStmt | ccTy == TailCall = [Return (LMLocalVar "void" LMVoid)]
                | otherwise        = []

    -- make the actual call
    case retTy of
        LMVoid -> do
            let s1 = Expr $ Call ccTy fptr argVars
            return (env2, stmts1 ++ stmts2 ++ [s1] ++ retStmt, top1 ++ top2)

        _ -> do
            let (creg, _) = ret_reg res
            (v1, s1) <- doExpr retTy $ Call ccTy fptr argVars
            let (env3, vreg, stmts3, top3) = genCmmReg env2 (CmmLocal creg)
            if retTy == pLower (getVarType vreg)
                then do
                    let s2 = Store v1 vreg
                    return (env3, stmts1 ++ stmts2 ++ stmts3 ++ [s1,s2]
                            ++ retStmt, top1 ++ top2 ++ top3)
                else do
                    let ty = pLower $ getVarType vreg
                    let op = case ty of
                            vt | isPointer vt -> LM_Bitcast
                               | isInt     vt -> LM_Ptrtoint
                               | otherwise    ->
                                   panic $ "genCall: CmmReg bad match for"
                                        ++ " returned type!"

                    (v2, s2) <- doExpr ty $ Cast op v1 ty
                    let s3 = Store v2 vreg
                    return (env3, stmts1 ++ stmts2 ++ stmts3 ++ [s1,s2,s3]
                            ++ retStmt, top1 ++ top2 ++ top3)


-- Conversion of call arguments.
arg_vars :: LlvmEnv -> HintedCmmActuals -> ([LlvmVar], [LlvmStatement], [LlvmCmmTop])
         -> UniqSM (LlvmEnv, [LlvmVar], [LlvmStatement], [LlvmCmmTop])

arg_vars env [] (vars, stmts, tops)
  = return (env, vars, stmts, tops)

arg_vars env (CmmHinted e AddrHint:rest) (vars, stmts, tops)
  = do (env', v1, stmts', top') <- exprToVar env e
       let op = case getVarType v1 of
               ty | isPointer ty -> LM_Bitcast
               ty | isInt ty     -> LM_Inttoptr

               a  -> panic $ "genCall: Can't cast llvmType to i8*! ("
                           ++ show a ++ ")"

       (v2, s1) <- doExpr (pLift i8) $ Cast op v1 (pLift i8)
       arg_vars env' rest (vars ++ [v2], stmts ++ stmts' ++ [s1], tops ++ top')

arg_vars env (CmmHinted e _:rest) (vars, stmts, tops)
  = do (env', v1, stmts', top') <- exprToVar env e
       arg_vars env' rest (vars ++ [v1], stmts ++ stmts', tops ++ top')

-- Decide what C function to use to implement a CallishMachOp
cmmPrimOpFunctions :: CallishMachOp -> FastString
cmmPrimOpFunctions mop
 = case mop of
    MO_F32_Exp    -> fsLit "expf"
    MO_F32_Log    -> fsLit "logf"
    MO_F32_Sqrt   -> fsLit "sqrtf"
    MO_F32_Pwr    -> fsLit "powf"

    MO_F32_Sin    -> fsLit "sinf"
    MO_F32_Cos    -> fsLit "cosf"
    MO_F32_Tan    -> fsLit "tanf"

    MO_F32_Asin   -> fsLit "asinf"
    MO_F32_Acos   -> fsLit "acosf"
    MO_F32_Atan   -> fsLit "atanf"

    MO_F32_Sinh   -> fsLit "sinhf"
    MO_F32_Cosh   -> fsLit "coshf"
    MO_F32_Tanh   -> fsLit "tanhf"

    MO_F64_Exp    -> fsLit "exp"
    MO_F64_Log    -> fsLit "log"
    MO_F64_Sqrt   -> fsLit "sqrt"
    MO_F64_Pwr    -> fsLit "pow"

    MO_F64_Sin    -> fsLit "sin"
    MO_F64_Cos    -> fsLit "cos"
    MO_F64_Tan    -> fsLit "tan"

    MO_F64_Asin   -> fsLit "asin"
    MO_F64_Acos   -> fsLit "acos"
    MO_F64_Atan   -> fsLit "atan"

    MO_F64_Sinh   -> fsLit "sinh"
    MO_F64_Cosh   -> fsLit "cosh"
    MO_F64_Tanh   -> fsLit "tanh"

    a -> panic $ "cmmPrimOpFunctions: Unknown callish op! (" ++ show a ++ ")"


-- | Tail function calls
--
genJump :: LlvmEnv -> CmmExpr -> UniqSM StmtData

-- Call to known function
genJump env (CmmLit (CmmLabel lbl)) = do
    (env', vf, stmts, top) <- getHsFunc env lbl
    let s1 = Expr $ Call TailCall vf []
    let s2 = Return (LMLocalVar "void" LMVoid)
    return (env', stmts ++ [s1,s2], top)

-- Call to unknown function / address
genJump env expr = do
    let fty = llvmFunTy
    (env', vf, stmts, top) <- exprToVar env expr

    let cast = case getVarType vf of
         ty | isPointer ty -> LM_Bitcast
         ty | isInt ty     -> LM_Inttoptr

         ty -> panic $ "genJump: Expr is of bad type for function call! ("
                     ++ show (ty) ++ ")"

    (v1, s1) <- doExpr (pLift fty) $ Cast cast vf (pLift fty)
    let s2 = Expr $ Call TailCall v1 []
    let s3 = Return (LMLocalVar "void" LMVoid)
    return (env', stmts ++ [s1,s2,s3], top)


-- | CmmAssign operation
--
-- We use stack allocated variables for CmmReg. The optimiser will replace
-- these with registers when possible.
genAssign :: LlvmEnv -> CmmReg -> CmmExpr -> UniqSM StmtData
genAssign _ (CmmGlobal _) _
 = panic $ "genAssign: The LLVM Back-end doesn't support the use of real "
        ++ "registers for STG registers. Use an unregistered build instead. "
        ++ "They should have been removed earlier in the code generation!"

genAssign env reg val = do
    let (env1, vreg, stmts1, top1) = genCmmReg env reg
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
            (vptr, s1) <- doExpr vty $ Cast LM_Inttoptr vaddr vty
            let s2 = Store vval vptr
            return (env2, stmts1 ++ stmts2 ++ [s1, s2], top1 ++ top2)

        else
            panic $ "genStore: ptr not of word size! (" ++ show vaddr ++ ")"


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
    (env', vc, stmts, top) <- exprToVarOpt env i1Option cond
    if getVarType vc == i1
        then do
            let stmt1 = BranchIf vc labelT labelF
            let stmt2 = MkLabel idF
            return $ (env', stmts ++ [stmt1, stmt2], top)
        else
            panic $ "genCondBranch: Cond expr not bool! (" ++ show vc ++ ")"


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

-- An expression conversion return.
--   * LlvmEnv: The new enviornment
--   * LlvmVar: The var holding the result of the expression
--   * LlvmStatements: Any statements needed to evaluate the expression
--   * LlvmCmmTop: Any global data needed for this expression
type ExprData = (LlvmEnv, LlvmVar, LlvmStatements, [LlvmCmmTop])

-- | Values which can be passed to 'exprToVar' to configure its
-- behaviour in certain circumstances.
data EOption = EOption {
        -- | The expected LlvmType for the returned variable.
        --
        -- Currently just used for determining if a comparison should return
        -- a boolean (i1) or a int (i32/i64).
        eoExpectedType :: Maybe LlvmType
  }

i1Option :: EOption
i1Option = EOption (Just i1)

wordOption :: EOption
wordOption = EOption (Just llvmWord)


-- | Convert a CmmExpr to a list of LlvmStatements with the result of the
--   expression being stored in the returned LlvmVar.
exprToVar :: LlvmEnv -> CmmExpr -> UniqSM ExprData
exprToVar env = exprToVarOpt env wordOption

exprToVarOpt :: LlvmEnv -> EOption -> CmmExpr -> UniqSM ExprData
exprToVarOpt env opt e = case e of

    CmmLit lit
        -> genLit env lit

    CmmLoad e' ty
        -> genCmmLoad env e' ty

    -- Cmmreg in expression is the value, so must load. If you want actual
    -- reg pointer, call genCmmReg directly.
    CmmReg r -> do
        let (env', vreg, stmts, top) = genCmmReg env r
        (v1, s1) <- doExpr (pLower $ getVarType vreg) $ Load vreg
        return (env', v1, stmts ++ [s1], top)

    CmmMachOp op exprs
        -> genMachOp env opt op exprs

    CmmRegOff r i
        -> exprToVar env $ expandCmmReg (r, i)

    CmmStackSlot _ _
        -> panic "exprToVar: CmmStackSlot not supported!"


-- | Handle CmmMachOp expressions
genMachOp :: LlvmEnv -> EOption -> MachOp -> [CmmExpr] -> UniqSM ExprData

-- Unary Machop
genMachOp env _ op [x] = case op of

    MO_Not w ->
        let all1 = mkIntLit (-1::Int) (widthToLlvmInt w)
        in negate (widthToLlvmInt w) all1 LM_MO_Xor

    MO_S_Neg w ->
        let all0 = mkIntLit (0::Int) (widthToLlvmInt w)
        in negate (widthToLlvmInt w) all0 LM_MO_Sub

    MO_F_Neg w ->
        let all0 = LMLitVar $ LMFloatLit 0 (widthToLlvmFloat w)
        in negate (widthToLlvmFloat w) all0 LM_MO_Sub

    MO_SF_Conv _ w -> fiConv (widthToLlvmFloat w) LM_Sitofp
    MO_FS_Conv _ w -> fiConv (widthToLlvmInt w) LM_Fptosi

    MO_SS_Conv from to
        -> sameConv from (widthToLlvmInt to) LM_Trunc LM_Sext

    MO_UU_Conv from to
        -> sameConv from (widthToLlvmInt to) LM_Trunc LM_Zext

    MO_FF_Conv from to
        -> sameConv from (widthToLlvmFloat to) LM_Fptrunc LM_Fpext

    a -> panic $ "genMachOp: unmatched unary CmmMachOp! (" ++ show a ++ ")"

    where
        negate ty v2 negOp = do
            (env', vx, stmts, top) <- exprToVar env x
            (v1, s1) <- doExpr ty $ LlvmOp negOp v2 vx
            return (env', v1, stmts ++ [s1], top)

        fiConv ty convOp = do
            (env', vx, stmts, top) <- exprToVar env x
            (v1, s1) <- doExpr ty $ Cast convOp vx ty
            return (env', v1, stmts ++ [s1], top)

        sameConv from ty reduce expand = do
            (env', vx, stmts, top) <- exprToVar env x
            let op = if widthInBits from > llvmWidthInBits ty
                        then reduce else expand
            (v1, s1) <- doExpr ty $ Cast op vx ty
            return (env', v1, stmts ++ [s1], top)


-- Binary MachOp
genMachOp env opt op [x, y] = case op of

    MO_Eq _   -> genBinComp opt LM_CMP_Eq
    MO_Ne _   -> genBinComp opt LM_CMP_Ne

    MO_S_Gt _ -> genBinComp opt LM_CMP_Sgt
    MO_S_Ge _ -> genBinComp opt LM_CMP_Sge
    MO_S_Lt _ -> genBinComp opt LM_CMP_Slt
    MO_S_Le _ -> genBinComp opt LM_CMP_Sle

    MO_U_Gt _ -> genBinComp opt LM_CMP_Ugt
    MO_U_Ge _ -> genBinComp opt LM_CMP_Uge
    MO_U_Lt _ -> genBinComp opt LM_CMP_Ult
    MO_U_Le _ -> genBinComp opt LM_CMP_Ule

    MO_Add _ -> genBinMach LM_MO_Add
    MO_Sub _ -> genBinMach LM_MO_Sub
    MO_Mul _ -> genBinMach LM_MO_Mul

    MO_U_MulMayOflo _ -> panic "genMachOp: MO_U_MulMayOflo unsupported!"

    MO_S_MulMayOflo w -> isSMulOK w x y

    MO_S_Quot _ -> genBinMach LM_MO_SDiv
    MO_S_Rem  _ -> genBinMach LM_MO_SRem

    MO_U_Quot _ -> genBinMach LM_MO_UDiv
    MO_U_Rem  _ -> genBinMach LM_MO_URem

    MO_F_Eq _ -> genBinComp opt LM_CMP_Feq
    MO_F_Ne _ -> genBinComp opt LM_CMP_Fne
    MO_F_Gt _ -> genBinComp opt LM_CMP_Fgt
    MO_F_Ge _ -> genBinComp opt LM_CMP_Fge
    MO_F_Lt _ -> genBinComp opt LM_CMP_Flt
    MO_F_Le _ -> genBinComp opt LM_CMP_Fle

    MO_F_Add  _ -> genBinMach LM_MO_Add
    MO_F_Sub  _ -> genBinMach LM_MO_Sub
    MO_F_Mul  _ -> genBinMach LM_MO_Mul
    MO_F_Quot _ -> genBinMach LM_MO_FDiv

    MO_And _   -> genBinMach LM_MO_And
    MO_Or  _   -> genBinMach LM_MO_Or
    MO_Xor _   -> genBinMach LM_MO_Xor
    MO_Shl _   -> genBinMach LM_MO_Shl
    MO_U_Shr _ -> genBinMach LM_MO_LShr
    MO_S_Shr _ -> genBinMach LM_MO_AShr

    a -> panic $ "genMachOp: unmatched binary CmmMachOp! (" ++ show a ++ ")"

    where
        binLlvmOp ty binOp = do
            (env1, vx, stmts1, top1) <- exprToVar env x
            (env2, vy, stmts2, top2) <- exprToVar env1 y
            if getVarType vx == getVarType vy
                then do
                    (v1, s1) <- doExpr (ty vx) $ binOp vx vy
                    return (env2, v1, stmts1 ++ stmts2 ++ [s1], top1 ++ top2)
                else do
                    let dx = Comment $ lines.show.llvmSDoc.PprCmm.pprExpr $ x
                    let dy = Comment $ lines.show.llvmSDoc.PprCmm.pprExpr $ y
                    (v1, s1) <- doExpr (ty vx) $ binOp vx vy
                    return (env2, v1, stmts1 ++ stmts2 ++ [dx,dy,s1], top1 ++ top2)

                    -- let o = case binOp vx vy of
                    --         Compare op _ _ -> show op
                    --         LlvmOp  op _ _ -> show op
                    --         _              -> "unknown"
                    -- panic $ "genMachOp: comparison between different types! ("
                    --         ++ o ++ " "++ show vx ++ ", " ++ show vy ++ ")"
                    --         ++ "\ne1: " ++ (show.llvmSDoc.PprCmm.pprExpr $ x)
                    --         ++ "\ne2: " ++ (show.llvmSDoc.PprCmm.pprExpr $ y)

        -- | Need to use EOption here as Cmm expects word size results from
        -- comparisons while llvm return i1. Need to extend to llvmWord type
        -- if expected
        genBinComp opt cmp = do
            ed@(env', v1, stmts, top) <- binLlvmOp (\_ -> i1) $ Compare cmp

            if getVarType v1 == i1
                then
                    case eoExpectedType opt of
                         Nothing ->
                             return ed

                         Just t | t == i1 ->
                                    return ed

                                | isInt t -> do
                                    (v2, s1) <- doExpr t $ Cast LM_Zext v1 t
                                    return (env', v2, stmts ++ [s1], top)

                                | otherwise ->
                                    panic $ "genBinComp: Can't case i1 compare"
                                        ++ "res to non int type " ++ show (t)
                else
                    panic $ "genBinComp: Compare returned type other then i1! "
                        ++ (show $ getVarType v1)

        genBinMach op = binLlvmOp getVarType (LlvmOp op)

        -- | Detect if overflow will occur in signed multiply of the two
        --   CmmExpr's. This is the LLVM assembly equivalent of the NCG
        --   implementation. Its much longer due to type information/safety.
        --   This should actually compile to only about 3 asm instructions.
        isSMulOK :: Width -> CmmExpr -> CmmExpr -> UniqSM ExprData
        isSMulOK _ x y = do
            (env1, vx, stmts1, top1) <- exprToVar env x
            (env2, vy, stmts2, top2) <- exprToVar env1 y

            let word  = getVarType vx
            let word2 = LMInt $ 2 * (llvmWidthInBits $ getVarType vx)
            let shift = llvmWidthInBits word
            let shift1 = mkIntLit (shift - 1) llvmWord
            let shift2 = mkIntLit shift llvmWord

            if isInt word
                then do
                    (x1, s1)     <- doExpr word2 $ Cast LM_Sext vx word2
                    (y1, s2)     <- doExpr word2 $ Cast LM_Sext vy word2
                    (r1, s3)     <- doExpr word2 $ LlvmOp LM_MO_Mul x1 y1
                    (rlow1, s4)  <- doExpr word $ Cast LM_Trunc r1 word
                    (rlow2, s5)  <- doExpr word $ LlvmOp LM_MO_AShr rlow1 shift1
                    (rhigh1, s6) <- doExpr word2 $ LlvmOp LM_MO_AShr r1 shift2
                    (rhigh2, s7) <- doExpr word $ Cast LM_Trunc rhigh1 word
                    (dst, s8)    <- doExpr word $ LlvmOp LM_MO_Sub rlow2 rhigh2

                    return (env2, dst, stmts1 ++ stmts2 ++
                            [s1, s2, s3, s4, s5, s6, s7, s8], top1 ++ top2)

                else
                    panic $ "isSMulOK: Not bit type! (" ++ show word ++ ")"


-- More then two expression, invalid!
genMachOp _ _ _ _ = panic "genMachOp: More then 2 expressions in MachOp!"


-- | Handle CmmLoad expression
genCmmLoad :: LlvmEnv -> CmmExpr -> CmmType -> UniqSM ExprData
genCmmLoad env e ty = do
    (env', iptr, stmts, tops) <- exprToVar env e
    let ety = getVarType iptr
    case (isInt ety) of
         True | llvmPtrBits == llvmWidthInBits ety ->  do
                    let pty = LMPointer $ cmmToLlvmType ty
                    (ptr, cast)  <- doExpr pty $ Cast LM_Inttoptr iptr pty
                    (dvar, load) <- doExpr (cmmToLlvmType ty) $ Load ptr
                    return (env', dvar, stmts ++ [cast, load], tops)

              | otherwise
                -> panic $ "exprToVar: can't cast to pointer as int not of"
                        ++ " pointer size!"

         False -> panic "exprToVar: CmmLoad expression is not of type int!"


-- | Handle CmmReg expression
--
-- We allocate CmmReg on the stack. This avoids having to map a CmmReg to an
-- equivalent SSA form and avoids having to deal with Phi node insertion.
-- This is also the approach llvm-gcc takes for C variables. The LLVM optimiser
-- can optimise this code to Phi form.
genCmmReg :: LlvmEnv -> CmmReg -> ExprData
genCmmReg env r@(CmmLocal (LocalReg un _))
  = let name = uniqToStr un
        oty  = Map.lookup name env

        (newv, stmts) = allocReg r
        -- FIX: Should remove from env or put in proc only env. This env is
        -- global to module and shouldn't contain local vars. Could also strip
        -- local vars from env at end of proc generation.
        nenv = Map.insert name (pLower $ getVarType newv) env
    in case oty of
            Just ety -> (env, (LMLocalVar name $ pLift ety), [], [])
            Nothing  -> (nenv, newv, stmts, [])

genCmmReg _ _ = panic $ "genCmmReg: Global reg encountered! Registered build"
                    ++ " not supported!"


-- | Allocate a CmmReg on the stack
allocReg :: CmmReg -> (LlvmVar, [LlvmStatement])
allocReg (CmmLocal (LocalReg un ty))
  = let ty' = cmmToLlvmType ty
        var = LMLocalVar (uniqToStr un) (LMPointer ty')
        alc = Alloca ty' 1
    in (var, [Assignment var alc])

allocReg _ = panic $ "allocReg: Global reg encountered! Registered build not"
                    ++ " supported!"


-- | Generate code for a literal
genLit :: LlvmEnv -> CmmLit -> UniqSM ExprData
genLit env (CmmInt i w)
  = return (env, mkIntLit i (LMInt $ widthInBits w), [], [])

genLit env (CmmFloat r w)
  = return (env, LMLitVar $ LMFloatLit r (widthToLlvmFloat w), [], [])

genLit env cmm@(CmmLabel l)
  = let label = strCLabel_llvm l
        ty = Map.lookup label env
        lmty = cmmToLlvmType $ cmmLitType cmm
    in case ty of
            -- Make generic external label defenition and then pointer to it
            Nothing -> do
                let glob@(var, _) = genStringLabelRef label
                let ldata = [CmmData Data [([glob], [])]]
                let env' = Map.insert label (pLower $ getVarType var) env
                (v1, s1) <- doExpr lmty $ Cast LM_Ptrtoint var llvmWord
                return (env', v1, [s1], ldata)
            -- Referenced data exists in this module, retrieve type and make
            -- pointer to it.
            Just ty' -> do
                let var = LMGlobalVar label (LMPointer ty') ExternallyVisible
                (v1, s1) <- doExpr lmty $ Cast LM_Ptrtoint var llvmWord
                return (env, v1, [s1], [])

genLit env (CmmLabelOff label off) = do
    (env', vlbl, stmts, stat) <- genLit env (CmmLabel label)
    let voff = mkIntLit off llvmWord
    (v1, s1) <- doExpr (getVarType vlbl) $ LlvmOp LM_MO_Add vlbl voff
    return (env', v1, stmts ++ [s1], stat)

genLit env (CmmLabelDiffOff l1 l2 off) = do
    (env1, vl1, stmts1, stat1) <- genLit env (CmmLabel l1)
    (env2, vl2, stmts2, stat2) <- genLit env1 (CmmLabel l2)
    let voff = mkIntLit off llvmWord
    let ty1 = getVarType vl1
    let ty2 = getVarType vl2
    if (isInt ty1) && (isInt ty2)
       && (llvmWidthInBits ty1 == llvmWidthInBits ty2)

       then do
            (v1, s1) <- doExpr (getVarType vl1) $ LlvmOp LM_MO_Sub vl1 vl2
            (v2, s2) <- doExpr (getVarType v1 ) $ LlvmOp LM_MO_Add v1 voff
            return (env2, v2, stmts1 ++ stmts2 ++ [s1, s2], stat1 ++ stat2)

        else
            panic "genLit: CmmLabelDiffOff encountered with different label ty!"

genLit env (CmmBlock b)
  = genLit env (CmmLabel $ infoTblLbl b)

genLit _ CmmHighStackMark
  = panic "genStaticLit - CmmHighStackMark unsupported!"


-- -----------------------------------------------------------------------------
-- Misc
--

-- | Get a function pointer to the CLabel specified.
--
-- This is for Haskell functions, function type is assumed, so doesn't work
-- with foreign functions.
getHsFunc :: LlvmEnv -> CLabel -> UniqSM ExprData
getHsFunc env lbl
  = let ty  = Map.lookup fn env
        fn  = strCLabel_llvm lbl
        def = llvmFunSig lbl ExternallyVisible
        fty = LMFunction def

    in case ty of

        Just ty'@(LMFunction sig) -> do
        -- Function in module in right form
            let fun = LMGlobalVar fn ty' (funcLinkage sig)
            return (env, fun, [], [])

        Just ty' -> do
        -- label in module but not function pointer, convert
            let fun = LMGlobalVar fn ty' ExternallyVisible
            (v1, s1) <- doExpr (pLift fty) $ Cast LM_Bitcast fun (pLift fty)
            return (env, v1, [s1], [])

        Nothing  -> do
        -- label not in module, create external reference
            let fun = LMGlobalVar fn fty ExternallyVisible
            let top = CmmData Data [([],[fty])]
            let env' = Map.insert fn fty env
            return (env', fun, [], [top])


-- | Create a new local var
mkLocalVar :: LlvmType -> UniqSM LlvmVar
mkLocalVar ty = do
    str <- mkUniqStr
    return $ LMLocalVar str ty


-- | Execute an expression, assigning result to a var
doExpr :: LlvmType -> LlvmExpression -> UniqSM (LlvmVar, LlvmStatement)
doExpr ty expr = do
    v <- mkLocalVar ty
    return (v, Assignment v expr)


-- | Expand CmmRegOff
expandCmmReg :: (CmmReg, Int) -> CmmExpr
expandCmmReg (reg, off)
  = let width = typeWidth (cmmRegType reg)
        voff  = CmmLit $ CmmInt (fromIntegral off) width
    in CmmMachOp (MO_Add width) [CmmReg reg, voff]


-- | Convert a block id into a appropriate Llvm label
blockIdToLlvm :: BlockId -> LlvmVar
blockIdToLlvm (BlockId id) = LMLocalVar (uniqToStr id) LMLabel


-- | Create a new uniq LMString
mkUniqStr :: UniqSM LMString
mkUniqStr = do
    us <- getUniqueUs
    return $ uniqToStr us


-- | Convert a Unique to a corresponding LMString representation.
uniqToStr :: Unique -> LMString
uniqToStr u = (show . llvmSDoc . ppr) u


-- | Create Llvm int Literal
mkIntLit :: Integral a => a -> LlvmType -> LlvmVar
mkIntLit i ty = LMLitVar $ LMIntLit (toInteger i) ty


-- | error function
panic :: String -> a
panic s = Outputable.panic $ "LlvmCodeGen.CodeGen." ++ s

