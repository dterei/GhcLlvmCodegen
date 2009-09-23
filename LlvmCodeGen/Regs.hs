-- ----------------------------------------------------------------------------
-- Deal with Cmm registers
-- 
-- Specifically, force use of register table (in memory), not pinned
-- hardware registers as the LLVM back-end doesn't support this.
--
 
module LlvmCodeGen.Regs ( get_GlobalReg_addr ) where

#include "HsVersions.h"

import Cmm
import qualified CgUtils ( get_GlobalReg_addr )

-- | We map STG registers onto appropriate CmmExprs.
--   If they map to don't map to a reg table offset then
--   Nothing is returned as the LLVM back-end doesn't
--   support pinned registers.
--
--   See also get_GlobalReg_addr in CgUtils.
--
get_GlobalReg_addr :: GlobalReg -> Either CmmExpr CmmExpr
get_GlobalReg_addr mid
  = let expr = CgUtils.get_GlobalReg_addr mid
    in case expr of
            CmmLit _  -> Right expr
            _         -> Left  expr

