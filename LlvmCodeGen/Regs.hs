-- ----------------------------------------------------------------------------
-- | Deal with Cmm registers
--

module LlvmCodeGen.Regs (

        RealReg, realRegsOrdered, getRealRegReg, getRealRegArg, getGlobalRegAddr

    ) where

#include "HsVersions.h"

import Llvm

import Cmm
import qualified CgUtils ( get_GlobalReg_addr )

-- | A data type corresponding to the STG virtual registers.
--
-- These provide a hard implementation of the STG virtual registers.
data RealReg
  = RR_Base
  | RR_Sp
  | RR_Hp
  | RR_R1
  | RR_R2
  | RR_R3
  | RR_R4
  | RR_R5
  | RR_R6
  | RR_SpLim
  | RR_F1
  | RR_F2
  | RR_F3
  | RR_F4
  | RR_D1
  | RR_D2
  deriving (Eq)

-- | Here is where the STG register map is defined for each target arch.
realRegsOrdered :: [RealReg]
realRegsOrdered
#ifdef NO_REGS
  = []

#elif defined(i386_TARGET_ARCH)
  = [RR_Base, RR_Sp, RR_Hp, RR_R1]

#elif defined(x86_64_TARGET_ARCH)
  = [RR_Base, RR_Sp, RR_Hp, RR_R1, RR_R2, RR_R3, RR_R4, RR_R5,
        RR_R6, RR_SpLim, RR_F1, RR_F2, RR_F3, RR_F4, RR_D1, RR_D2]

#else
  = [RR_Base, RR_Sp, RR_Hp, RR_R1, RR_R2, RR_R3, RR_R4, RR_R5,
        RR_R6, RR_SpLim, RR_F1, RR_F2, RR_F3, RR_F4, RR_D1, RR_D2]
#endif

-- | Get the LlvmVar storing the real register
getRealRegReg :: RealReg -> LlvmVar
getRealRegReg rr
  = case rr of
        RR_Base  -> lmBaseReg
        RR_Sp    -> lmSpReg
        RR_Hp    -> lmHpReg
        RR_R1    -> lmR1Reg
        RR_R2    -> lmR2Reg
        RR_R3    -> lmR3Reg
        RR_R4    -> lmR4Reg
        RR_R5    -> lmR5Reg
        RR_R6    -> lmR6Reg
        RR_SpLim -> lmSpLimReg
        RR_F1    -> lmF1Reg
        RR_F2    -> lmF2Reg
        RR_F3    -> lmF3Reg
        RR_F4    -> lmF4Reg
        RR_D1    -> lmD1Reg
        RR_D2    -> lmD2Reg


-- | Get the LlvmVar storing the argument for the real register
getRealRegArg :: RealReg -> LlvmVar
getRealRegArg rr
  = case rr of
        RR_Base  -> lmBaseArg
        RR_Sp    -> lmSpArg
        RR_Hp    -> lmHpArg
        RR_R1    -> lmR1Arg
        RR_R2    -> lmR2Arg
        RR_R3    -> lmR3Arg
        RR_R4    -> lmR4Arg
        RR_R5    -> lmR5Arg
        RR_R6    -> lmR6Arg
        RR_SpLim -> lmSpLimArg
        RR_F1    -> lmF1Arg
        RR_F2    -> lmF2Arg
        RR_F3    -> lmF3Arg
        RR_F4    -> lmF4Arg
        RR_D1    -> lmD1Arg
        RR_D2    -> lmD2Arg


-- * Llvm Version of STG registers.
-- HACK: could name conflict as not unique.

-- ** Register versions used in a function
lmBaseReg, lmSpReg, lmHpReg, lmR1Reg, lmR2Reg, lmR3Reg, lmR4Reg, lmR5Reg,
    lmR6Reg, lmSpLimReg, lmF1Reg, lmF2Reg, lmF3Reg, lmF4Reg, lmD1Reg,
    lmD2Reg:: LlvmVar

lmBaseReg  = LMLocalVar "stg_terei_baseReg"  llvmWordPtr
lmSpReg    = LMLocalVar "stg_terei_spReg"    llvmWordPtr
lmHpReg    = LMLocalVar "stg_terei_hpReg"    llvmWordPtr
lmR1Reg    = LMLocalVar "stg_terei_r1Reg"    llvmWordPtr
lmR2Reg    = LMLocalVar "stg_terei_r2Reg"    llvmWordPtr
lmR3Reg    = LMLocalVar "stg_terei_r3Reg"    llvmWordPtr
lmR4Reg    = LMLocalVar "stg_terei_r4Reg"    llvmWordPtr
lmR5Reg    = LMLocalVar "stg_terei_r5Reg"    llvmWordPtr
lmR6Reg    = LMLocalVar "stg_terei_r6Reg"    llvmWordPtr
lmSpLimReg = LMLocalVar "stg_terei_spLimReg" llvmWordPtr
lmF1Reg    = LMLocalVar "stg_terei_f1Reg"  $ pLift LMFloat
lmF2Reg    = LMLocalVar "stg_terei_f2Reg"  $ pLift LMFloat
lmF3Reg    = LMLocalVar "stg_terei_f3Reg"  $ pLift LMFloat
lmF4Reg    = LMLocalVar "stg_terei_f4Reg"  $ pLift LMFloat
lmD1Reg    = LMLocalVar "stg_terei_d1Reg"  $ pLift LMDouble
lmD2Reg    = LMLocalVar "stg_terei_d2Reg"  $ pLift LMDouble

-- ** Argument versions used in function as arguments to pass registers
lmBaseArg, lmSpArg, lmHpArg, lmR1Arg, lmR2Arg, lmR3Arg, lmR4Arg, lmR5Arg,
    lmR6Arg, lmSpLimArg, lmF1Arg, lmF2Arg, lmF3Arg, lmF4Arg, lmD1Arg,
    lmD2Arg:: LlvmVar

lmBaseArg  = LMLocalVar "stg_terei_baseArg"  llvmWord
lmSpArg    = LMLocalVar "stg_terei_spArg"    llvmWord
lmHpArg    = LMLocalVar "stg_terei_hpArg"    llvmWord
lmR1Arg    = LMLocalVar "stg_terei_r1Arg"    llvmWord
lmR2Arg    = LMLocalVar "stg_terei_r2Arg"    llvmWord
lmR3Arg    = LMLocalVar "stg_terei_r3Arg"    llvmWord
lmR4Arg    = LMLocalVar "stg_terei_r4Arg"    llvmWord
lmR5Arg    = LMLocalVar "stg_terei_r5Arg"    llvmWord
lmR6Arg    = LMLocalVar "stg_terei_r6Arg"    llvmWord
lmSpLimArg = LMLocalVar "stg_terei_spLimArg" llvmWord
lmF1Arg    = LMLocalVar "stg_terei_f1Arg"    LMFloat
lmF2Arg    = LMLocalVar "stg_terei_f2Arg"    LMFloat
lmF3Arg    = LMLocalVar "stg_terei_f3Arg"    LMFloat
lmF4Arg    = LMLocalVar "stg_terei_f4Arg"    LMFloat
lmD1Arg    = LMLocalVar "stg_terei_d1Arg"    LMDouble
lmD2Arg    = LMLocalVar "stg_terei_d2Arg"    LMDouble


-- | We map STG registers onto an appropriate 'RealReg' or 'CmmExpr'.
--
-- 'RealReg' is used when the STG register is pinned to a hardware register
-- and 'CmmExpr' is used when it isn't and instead a load from the Register
-- table is used.
--
-- See also 'getGlobalRegAddr' in 'CgUtils'.
--
getGlobalRegAddr :: GlobalReg -> Either RealReg CmmExpr
getGlobalRegAddr r@(BaseReg       ) = regsOrTable RR_Base r
getGlobalRegAddr r@(Sp            ) = regsOrTable RR_Sp r
getGlobalRegAddr r@(Hp            ) = regsOrTable RR_Hp r
getGlobalRegAddr r@(VanillaReg 1 _) = regsOrTable RR_R1 r
getGlobalRegAddr r@(VanillaReg 2 _) = regsOrTable RR_R2 r
getGlobalRegAddr r@(VanillaReg 3 _) = regsOrTable RR_R3 r
getGlobalRegAddr r@(VanillaReg 4 _) = regsOrTable RR_R4 r
getGlobalRegAddr r@(VanillaReg 5 _) = regsOrTable RR_R5 r
getGlobalRegAddr r@(VanillaReg 6 _) = regsOrTable RR_R6 r
getGlobalRegAddr r@(SpLim         ) = regsOrTable RR_SpLim r
getGlobalRegAddr r@(FloatReg   1  ) = regsOrTable RR_F1 r
getGlobalRegAddr r@(FloatReg   2  ) = regsOrTable RR_F2 r
getGlobalRegAddr r@(FloatReg   3  ) = regsOrTable RR_F3 r
getGlobalRegAddr r@(FloatReg   4  ) = regsOrTable RR_F4 r
getGlobalRegAddr r@(DoubleReg  1  ) = regsOrTable RR_D1 r
getGlobalRegAddr r@(DoubleReg  2  ) = regsOrTable RR_D2 r

getGlobalRegAddr r                  = Right $ CgUtils.get_GlobalReg_addr r

regsOrTable :: RealReg -> GlobalReg -> Either RealReg CmmExpr
regsOrTable lmR stgR = if lmR `elem` realRegsOrdered
         then Left lmR
         else Right $ CgUtils.get_GlobalReg_addr stgR

