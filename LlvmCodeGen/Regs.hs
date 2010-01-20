-- ----------------------------------------------------------------------------
-- Deal with Cmm registers
--
-- Specifically, force use of register table (in memory), not pinned
-- hardware registers as the LLVM back-end doesn't support this.
--

module LlvmCodeGen.Regs (

        RealReg(..),
        lmBaseReg, lmSpReg, lmHpReg, lmR1Reg,
        lmBaseArg, lmSpArg, lmHpArg, lmR1Arg,
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


-- Llvm Version of STG registers.
-- HACK: could name conflict as not uniqued.

-- reg versions used in a function
lmBaseReg, lmSpReg, lmHpReg, lmR1Reg :: LlvmVar
lmBaseReg = LMLocalVar "stg_terei_baseReg" llvmWordPtr
lmSpReg   = LMLocalVar "stg_terei_spReg"   llvmWordPtr
lmHpReg   = LMLocalVar "stg_terei_hpReg"   llvmWordPtr
lmR1Reg   = LMLocalVar "stg_terei_r1Reg"   llvmWordPtr

-- argument versions used in function as arguments to pass registers
lmBaseArg, lmSpArg, lmHpArg, lmR1Arg :: LlvmVar
lmBaseArg = LMLocalVar "stg_terei_baseArg" llvmWord
lmSpArg   = LMLocalVar "stg_terei_spArg"   llvmWord
lmHpArg   = LMLocalVar "stg_terei_hpArg"   llvmWord
lmR1Arg   = LMLocalVar "stg_terei_r1Arg"   llvmWord


getLlvmStgReg :: GlobalReg -> LlvmVar
getLlvmStgReg (BaseReg       ) = lmBaseReg
getLlvmStgReg (Sp            ) = lmSpReg
getLlvmStgReg (Hp            ) = lmHpReg
getLlvmStgReg (VanillaReg 1 _) = lmR1Reg
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
#else
get_GlobalReg_addr _ = panic "Only support i386 in registered mode currently"
#endif
#endif

get_GlobalReg_addr mid = Right $ CgUtils.get_GlobalReg_addr mid


-- | error function
panic :: String -> a
panic s = Outputable.panic $ "LlvmCodeGen.Regs." ++ s

