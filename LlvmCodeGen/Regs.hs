-- ----------------------------------------------------------------------------
-- Deal with Cmm registers
--
-- Specifically, force use of register table (in memory), not pinned
-- hardware registers as the LLVM back-end doesn't support this.
--

module LlvmCodeGen.Regs (

        RealReg(..), realRegsOrdered, getRealRegReg, getRealRegArg,
        lmBaseReg, lmSpReg, lmHpReg, lmR1Reg, lmR2Reg, lmR3Reg, lmR4Reg,
        lmR5Reg, lmR6Reg, lmSpLimReg, lmF1Reg, lmF2Reg, lmF3Reg, lmF4Reg,
        lmD1Reg, lmD2Reg,
        lmBaseArg, lmSpArg, lmHpArg, lmR1Arg, lmR2Arg, lmR3Arg, lmR4Arg,
        lmR5Arg, lmR6Arg, lmSpLimArg, lmF1Arg, lmF2Arg, lmF3Arg, lmF4Arg,
        lmD1Arg, lmD2Arg,
        getLlvmStgReg, get_GlobalReg_addr

    ) where

#include "HsVersions.h"

import Llvm

import Cmm
import qualified CgUtils ( get_GlobalReg_addr )
import qualified Outputable ( panic )

-- Currently a hack!
-- registered specific to x86 back-end and needs a custom version
-- of llvm which uses a modified call convention to pass the registers
-- around correctly.
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

realRegsOrdered :: [RealReg]
realRegsOrdered
  = [RR_Base, RR_Sp, RR_Hp, RR_R1, RR_R2, RR_R3, RR_R4, RR_R5,
        RR_R6, RR_SpLim, RR_F1, RR_F2, RR_F3, RR_F4, RR_D1, RR_D2]

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


-- Llvm Version of STG registers.
-- HACK: could name conflict as not uniqued.

-- reg versions used in a function
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

-- argument versions used in function as arguments to pass registers
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


getLlvmStgReg :: GlobalReg -> LlvmVar
getLlvmStgReg (BaseReg       ) = lmBaseReg
getLlvmStgReg (Sp            ) = lmSpReg
getLlvmStgReg (Hp            ) = lmHpReg
getLlvmStgReg (VanillaReg 1 _) = lmR1Reg
getLlvmStgReg (VanillaReg 2 _) = lmR2Reg
getLlvmStgReg (VanillaReg 3 _) = lmR3Reg
getLlvmStgReg (VanillaReg 4 _) = lmR4Reg
getLlvmStgReg (VanillaReg 5 _) = lmR5Reg
getLlvmStgReg (VanillaReg 6 _) = lmR6Reg
getLlvmStgReg (SpLim         ) = lmSpLimReg
getLlvmStgReg (FloatReg   1  ) = lmF1Reg
getLlvmStgReg (FloatReg   2  ) = lmF2Reg
getLlvmStgReg (FloatReg   3  ) = lmF3Reg
getLlvmStgReg (FloatReg   4  ) = lmF4Reg
getLlvmStgReg (DoubleReg  1  ) = lmD1Reg
getLlvmStgReg (DoubleReg  2  ) = lmD2Reg
getLlvmStgReg _ = panic $ "getLlvmStgReg: Unsupported global reg! only x86 is "
                    ++ "supported as a registered build!"


-- | We map STG registers onto appropriate CmmExprs.
--   If they map to don't map to a reg table offset then
--   Nothing is returned as the LLVM back-end doesn't
--   support pinned registers.
--
--   See also get_GlobalReg_addr in CgUtils.
--
get_GlobalReg_addr :: GlobalReg -> Either RealReg CmmExpr
#ifndef NO_REGS

#ifdef i386_TARGET_ARCH
get_GlobalReg_addr (BaseReg       ) = Left RR_Base
get_GlobalReg_addr (Sp            ) = Left RR_Sp
get_GlobalReg_addr (Hp            ) = Left RR_Hp
get_GlobalReg_addr (VanillaReg 1 _) = Left RR_R1
#endif

#ifdef x86_64_TARGET_ARCH
get_GlobalReg_addr (BaseReg       ) = Left RR_Base
get_GlobalReg_addr (Sp            ) = Left RR_Sp
get_GlobalReg_addr (Hp            ) = Left RR_Hp
get_GlobalReg_addr (VanillaReg 1 _) = Left RR_R1
get_GlobalReg_addr (VanillaReg 2 _) = Left RR_R2
get_GlobalReg_addr (VanillaReg 3 _) = Left RR_R3
get_GlobalReg_addr (VanillaReg 4 _) = Left RR_R4
get_GlobalReg_addr (VanillaReg 5 _) = Left RR_R5
get_GlobalReg_addr (VanillaReg 6 _) = Left RR_R6
get_GlobalReg_addr (SpLim         ) = Left RR_SpLim
get_GlobalReg_addr (FloatReg   1  ) = Left RR_F1
get_GlobalReg_addr (FloatReg   2  ) = Left RR_F2
get_GlobalReg_addr (FloatReg   3  ) = Left RR_F3
get_GlobalReg_addr (FloatReg   4  ) = Left RR_F4
get_GlobalReg_addr (DoubleReg  1  ) = Left RR_D1
get_GlobalReg_addr (DoubleReg  2  ) = Left RR_D2
#endif

#else
get_GlobalReg_addr _ = panic "Only support x86/x86-64 in registered mode currently"
#endif

get_GlobalReg_addr mid = Right $ CgUtils.get_GlobalReg_addr mid


-- | error function
panic :: String -> a
panic s = Outputable.panic $ "LlvmCodeGen.Regs." ++ s

