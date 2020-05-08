module HaskellWorks.Data.Bits.Branchless.TestBit.Word64
  ( testBit
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning

-- | Test the bit at the given index.
--
-- The position is zero-index.
--
-- >>> testBit 6 0
-- 0
-- >>> testBit 6 1
-- 1
-- >>> testBit 6 2
-- 1
-- >>> testBit 6 3
-- 0
testBit :: Word64 -> Position -> Word64
testBit w n = (w .>. toCount n) .&. 1
{-# INLINE testBit #-}
