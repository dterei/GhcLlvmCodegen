-- ----------------------------------------------------------------------------
-- | Deal with Cmm registers
--

module LlvmCodeGen.Regs (
        activeStgRegs, lmGlobalRegArg, lmGlobalRegVar
    ) where

#include "HsVersions.h"

import Llvm

import CmmExpr
import Outputable ( panic )

-- | Here is where the STG register map is defined for each target arch.
activeStgRegs :: [GlobalReg]
activeStgRegs
#ifdef NO_REGS
  = []

#elif defined(i386_TARGET_ARCH)
  = [BaseReg, Sp, Hp, VanillaReg 1 VGcPtr]

#elif defined(x86_64_TARGET_ARCH)
  = [BaseReg, Sp, Hp, VanillaReg 1 VGcPtr, VanillaReg 2 VGcPtr,
     VanillaReg 3 VGcPtr, VanillaReg 4 VGcPtr, VanillaReg 5 VGcPtr,
     VanillaReg 6 VGcPtr, SpLim, FloatReg 1, FloatReg 2, FloatReg 3,
     FloatReg 4, DoubleReg 1, DoubleReg 2]

#else
  = panic $ "LlvmCodeGen.Regs: Registered mode with LLVM back-end not "
        ++ "supported for this architecture"
#endif

-- | Get the LlvmVar function variable storing the real register
lmGlobalRegVar :: GlobalReg -> LlvmVar
lmGlobalRegVar = lmGlobalReg "_Reg"

-- | Get the LlvmVar function argument storing the real register
lmGlobalRegArg :: GlobalReg -> LlvmVar
lmGlobalRegArg = (pVarLower . lmGlobalReg "_Arg")

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

