-- ----------------------------------------------------------------------------
-- | This module supplies bindings to generate Llvm IR from Haskell
-- (<http://www.llvm.org/docs/LangRef.html>).
--
-- Note: this module is developed in a demand driven way. It is no complete
-- LLVM binding library in Haskell, but enough to generate code for GHC.
--
-- This code is derived from code taken from the Essential Haskell Compiler
-- (EHC) project (<http://www.cs.uu.nl/wiki/Ehc/WebHome>).
--

module Llvm (

        -- * Modules, Functions and Blocks
        LlvmModule(..),

        LlvmFunction(..), LlvmFunctionDecl(..),
        LlvmFunctions, LlvmFunctionDecls,
        LlvmStatement(..), LlvmExpression(..),
        LlvmStatements,
        LlvmBlocks, LlvmBlock(..), LlvmBlockId,

        -- * Call Handling
        LlvmCallConvention(..), LlvmCallType(..), LlvmParameterListType(..),
        LlvmLinkageType(..), LlvmFuncAttr(..),

        -- * Operations and Comparisons
        LlvmCmpOp(..), LlvmMachOp(..), LlvmCastOp(..),

        -- * Variables and Type System
        LlvmVar(..), LlvmStatic(..), LlvmLit(..), LlvmType(..),
        LMGlobal, LMString, LMConstant,

        -- ** Some basic types
        i64, i32, i16, i8, i1, llvmWord, llvmWordPtr,

        -- ** Operations on the type system.
        isGlobal, getLitType, getLit, getName, getPlainName, getVarType,
        getStatType, getGlobalVar, getGlobalType, pVarLower, pLift, pLower,
        isInt, isFloat, isPointer, llvmWidthInBits,

        -- * Pretty Printing
        ppLlvmModule, ppLlvmComments, ppLlvmComment, ppLlvmConstants,
        ppLlvmConstant, ppLlvmGlobals, ppLlvmGlobal, ppLlvmFunctionDecls,
        ppLlvmFunctionDecl, ppLlvmFunctions, ppLlvmFunction, ppLlvmType,
        ppLlvmTypes

    ) where

import Llvm.AbsSyn
import Llvm.PpLlvm
import Llvm.Types

