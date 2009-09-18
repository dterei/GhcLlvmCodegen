-- ----------------------------------------------------------------------------
-- This module supplies bindings to generate Llvm IR from Haskell 
-- (<http://www.llvm.org/docs/LangRef.html>). 
--    
-- Note: this module is developed in a demand driven way. It is no complete
--       Llvm binding library in Haskell, but enough to generate code for GHC.
-- 
-- This code was taken from the Essential Haskel Compiler (EHC) project.
--

module Llvm (

        LlvmModule(..),

        LlvmFunction(..), LlvmFunctionDecl(..),
        LlvmFunctions, LlvmFunctionDecls,
        LlvmStatement(..), LlvmExpression(..),
        LlvmStatements,
        LlvmBlocks, LlvmBlock(..), LlvmBlockId,

        LlvmCmpOp(..), LlvmMachOp(..), LlvmCastOp(..),
        LlvmVar(..), LlvmStatic(..), LlvmLit(..), LlvmType(..),

        LMGlobal, LMString, LMConstant,
        i64, i32, i16, i8, i1,
        isGlobal, getLitType, getLit, getName, getPlainName, getVarType,
        getStatType, getGlobalVar, getGlobalType, pVarLower, pLift, pLower,
        isInt, isFloat, isPointer, llvmWidthInBits,

        LlvmCallConvention(..), LlvmCallType(..), LlvmParameterListType(..),
        LlvmLinkageType(..), LlvmFuncAttr(..),

        ppLlvmModule, ppLlvmComments, ppLlvmComment, ppLlvmConstants,
        ppLlvmConstant, ppLlvmGlobals, ppLlvmGlobal, ppLlvmFunctionDecls,
        ppLlvmFunctionDecl, ppLlvmFunctions, ppLlvmFunction,

    ) where

 
import Llvm.AbsSyn
import Llvm.PpLlvm
import Llvm.Types

