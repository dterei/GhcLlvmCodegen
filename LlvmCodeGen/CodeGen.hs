-- ----------------------------------------------------------------------------
-- Handle conversion of CmmProc to LLVM code.
--
module LlvmCodeGen.CodeGen where

import Llvm
import LlvmCodeGen.Base

import Cmm
import CLabel

import FastString ( unpackFS )
import Outputable ( panic )
import UniqSupply

-- -----------------------------------------------------------------------------
-- Top-level of the llvm proc codegen
--
genLlvmProc :: RawCmmTop -> UniqSM [LlvmCmmTop]

genLlvmProc (CmmData _ _)
  = return []

genLlvmProc (CmmProc _ _ _ (ListGraph []))
  = return []

genLlvmProc (CmmProc info lbl _ (ListGraph blocks))
  = return []

-- -----------------------------------------------------------------------------
-- Block code generation
--
basicBlockCodeGen :: CmmBasicBlock -> UniqSM ( [LlvmBlock] , [LlvmCmmTop])
basicBlockCodeGen (BasicBlock id stmts)
  = do instrs <- stmtsToInstrs stmts
       return ([(BasicBlock id instrs)], [])


stmtsToInstrs :: [CmmStmt] -> UniqSM [LlvmStatement]
stmtsToInstrs stmts
   = do instrs <- mapM stmtToInstrs stmts
        return $ concat instrs


stmtToInstrs :: CmmStmt -> UniqSM [LlvmStatement]
stmtToInstrs stmt = case stmt of
    CmmNop       -> return []

    CmmComment s -> return [Comment [(unpackFS s)]]

    CmmAssign reg src
        | isFloatType ty -> return []
        | otherwise      -> return []
        where ty = cmmRegType reg

    CmmStore addr src
        | isFloatType ty -> return []
        | otherwise      -> return []
        where ty = cmmExprType src

    CmmCall target result_regs args _ _
        -> return []

    CmmBranch   id       -> return []
    CmmCondBranch arg id -> return []
    CmmSwitch   arg ids  -> return []
    CmmJump arg _        -> return []

    CmmReturn _       
     -> panic $ "LlvmCodeGen.CodeGen.stmtToInstrs: return statement should"
            ++ "have been cps'd away"

exprToVar :: CmmExpr -> UniqSM (Maybe LlvmVar, [LlvmStatement])
exprToVar e = case e of
    CmmLit lit         -> return (Nothing, [])

    CmmLoad e' ty      -> return (Nothing, [])

    CmmReg r           -> return (Nothing, [])
    
    CmmMachOp op exprs -> return (Nothing, [])

    CmmStackSlot _ _ 
        -> panic "LlvmCodeGen.CodeGen.exprToVar: CmmStackSlot not supported!"
    
    CmmRegOff r i
        -> exprToVar $ expandCmmReg (r, i)

-- -----------------------------------------------------------------------------
-- Misc
--

-- Expand CmmRegOff
expandCmmReg :: (CmmReg, Int) -> CmmExpr
expandCmmReg (reg, off)
	= CmmMachOp (MO_Add width) [CmmReg reg, CmmLit (CmmInt (fromIntegral off) width)]
	where width = typeWidth (cmmRegType reg)

