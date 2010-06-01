-- ----------------------------------------------------------------------------
-- | Deal with Cmm registers
--

module LlvmCodeGen.Regs (
        activeStgRegs, lmGlobalRegArg, lmGlobalRegVar
    ) where

#include "HsVersions.h"
#include "../includes/stg/MachRegs.h"

import Llvm

import CmmExpr
import Outputable ( panic )

-- | Here is where the STG register map is defined for each target arch.
-- The order matters! We must make sure to maintain the order here
-- with the order used in the LLVM calling conventions.
activeStgRegs :: [GlobalReg]
activeStgRegs = [
#ifdef REG_Base
    BaseReg
#endif
#ifdef REG_Sp 
    ,Sp
#endif 
#ifdef REG_Hp 
    ,Hp
#endif
#ifdef REG_R1
    ,VanillaReg 1 VGcPtr
#endif  
#ifdef REG_R2  
    ,VanillaReg 2 VGcPtr
#endif  
#ifdef REG_R3  
    ,VanillaReg 3 VGcPtr
#endif  
#ifdef REG_R4  
    ,VanillaReg 4 VGcPtr
#endif  
#ifdef REG_R5  
    ,VanillaReg 5 VGcPtr
#endif  
#ifdef REG_R6  
    ,VanillaReg 6 VGcPtr
#endif  
#ifdef REG_SpLim 
    ,SpLim
#endif 
#ifdef REG_F1
    ,FloatReg 1
#endif
#ifdef REG_F2
    ,FloatReg 2
#endif
#ifdef REG_F3
    ,FloatReg 3
#endif
#ifdef REG_F4
    ,FloatReg 4
#endif
#ifdef REG_D1
    ,DoubleReg 1
#endif
#ifdef REG_D2
    ,DoubleReg 2
#endif
    ]

-- | Get the LlvmVar function variable storing the real register
lmGlobalRegVar :: GlobalReg -> LlvmVar
lmGlobalRegVar = lmGlobalReg "_Var"

-- | Get the LlvmVar function argument storing the real register
lmGlobalRegArg :: GlobalReg -> LlvmVar
lmGlobalRegArg = (pVarLower . lmGlobalReg "_Arg")

{- Need to make sure the names here can't conflict with the unique generated
   names. Uniques generated names containing only base62 chars. So using say
    the '_' char guarantees this.
-}
lmGlobalReg :: String -> GlobalReg -> LlvmVar
lmGlobalReg suf rr
  = case rr of
        BaseReg        -> wordGlobal $ "Base" ++ suf
        Sp             -> wordGlobal $ "Sp" ++ suf
        Hp             -> wordGlobal $ "Hp" ++ suf
        VanillaReg 1 _ -> wordGlobal $ "R1" ++ suf
        VanillaReg 2 _ -> wordGlobal $ "R2" ++ suf
        VanillaReg 3 _ -> wordGlobal $ "R3" ++ suf
        VanillaReg 4 _ -> wordGlobal $ "R4" ++ suf
        VanillaReg 5 _ -> wordGlobal $ "R5" ++ suf
        VanillaReg 6 _ -> wordGlobal $ "R6" ++ suf
        SpLim          -> wordGlobal $ "SpLim" ++ suf
        FloatReg 1     -> floatGlobal $"F1" ++ suf
        FloatReg 2     -> floatGlobal $"F2" ++ suf
        FloatReg 3     -> floatGlobal $"F3" ++ suf
        FloatReg 4     -> floatGlobal $"F4" ++ suf
        DoubleReg 1    -> doubleGlobal $ "D1" ++ suf
        DoubleReg 2    -> doubleGlobal $ "D2" ++ suf
    where
        wordGlobal   name = LMLocalVar name llvmWordPtr
        floatGlobal  name = LMLocalVar name $ pLift LMFloat
        doubleGlobal name = LMLocalVar name $ pLift LMDouble

