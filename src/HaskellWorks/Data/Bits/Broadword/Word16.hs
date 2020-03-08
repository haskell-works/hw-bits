{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Bits.Broadword.Word16
  ( h
  , l
  , μ0
  , μ1
  , μ2
  , μ3
  , μ4
  , kBitDiff
  , kBitDiffPos
  , kBitDiffUnsafe
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise

-- | Initialise all sub-words of size k where 'k' ∈ { 2, 4, 8, 16 } such that the lowest bit is set to 1 and all other bits are cleared.
--
-- >>> import Numeric(showHex)
-- >>> showHex (l 2) ""
-- "5555"
-- >>> showHex (l 4) ""
-- "1111"
-- >>> showHex (l 8) ""
-- "101"
-- >>> showHex (l 16) ""
-- "1"
l :: Int -> Word16
l 2  = 0x5555
l 4  = 0x1111
l 8  = 0x0101
l 16 = 0x0001
l k  = error ("Invalid h k where k = " ++ show k)
{-# INLINE l #-}

-- | Initialise all sub-words of size k where 'k' ∈ { 2, 4, 8, 16 } such that the highest bit is set to 1 and all other bits are cleared.
--
-- >>> import Numeric(showHex)
-- >>> showHex (h 2) ""
-- "aaaa"
-- >>> showHex (h 4) ""
-- "8888"
-- >>> showHex (h 8) ""
-- "8080"
-- >>> showHex (h 16) ""
-- "8000"
h :: Int -> Word16
h 2  = 0xaaaa
h 4  = 0x8888
h 8  = 0x8080
h 16 = 0x8000
h k  = error ("Invalid h k where k = " ++ show k)
{-# INLINE h #-}

-- | Broadword subtraction of sub-words of size 'k' where 'k' ∈ { 2, 4, 8, 16 }.
--
-- The subtraction respects 2's complement so sub-words may be regarded as signed or unsigned words.
--
-- >>> import Numeric(showHex)
-- >>> showHex (kBitDiff 8 0x0201 0x0101) ""
-- "100"
-- >>> showHex (kBitDiff 8 0x0201 0x0102) ""
-- "1ff"
-- >>> showHex (kBitDiff 8 0x20ff 0x10ff) ""
-- "1000"
kBitDiff :: Int -> Word16 -> Word16 -> Word16
kBitDiff k x y = ((x .|. h k) - (y .&. comp (h k))) .^. ((x .^. comp y) .&. h k)
{-# INLINE kBitDiff #-}

-- | Broadword subtraction of sub-words of size 'k' where 'k' ∈ { 2, 4, 8, 16 } where results are bounded from below by 0.
--
-- >>> import Numeric(showHex)
-- >>> showHex (kBitDiffPos 8 0x0201 0x0101) ""
-- "100"
-- >>> showHex (kBitDiffPos 8 0x0201 0x0102) ""
-- "100"
-- >>> showHex (kBitDiffPos 8 0x20ff 0x10ff) ""
-- "1000"
kBitDiffPos :: Int -> Word16 -> Word16 -> Word16
kBitDiffPos k x y = let d = kBitDiff k x y in d .&. kBitDiff k 0 ((comp d .&. h 8) .>. fromIntegral (k - 1))
{-# INLINE kBitDiffPos #-}

-- | Broadword subtraction of sub-words of size 'k' where 'k' ∈ { 2, 4, 8, 16, 32, 64 } where all the sub-words of 'x' and 'y' must
-- not have the signed bit set for the result to be meaningful.
--
-- >>> import Numeric(showHex)
-- >>> showHex (kBitDiffUnsafe 8 0x0201 0x0101) ""
-- "100"
-- >>> showHex (kBitDiffUnsafe 8 0x0201 0x0102) ""
-- "1ff"
-- >>> showHex (kBitDiffUnsafe 8 0x20ff 0x10ff) "" -- produces nonsense in the last sub-word
-- "1080"
kBitDiffUnsafe :: Int -> Word16 -> Word16 -> Word16
kBitDiffUnsafe k x y = ((x .|. h k) - y) .^. h k
{-# INLINE kBitDiffUnsafe #-}

-- | Subwords of size 2 ^ 0 alternating between all bits cleared and all bits
μ0 :: Word16
μ0 = 0x5555
{-# INLINE μ0 #-}

-- | Subwords of size 2 ^ 1 alternating between all bits cleared and all bits
μ1 :: Word16
μ1 = 0x3333
{-# INLINE μ1 #-}

-- | Subwords of size 2 ^ 2 alternating between all bits cleared and all bits
μ2 :: Word16
μ2 = 0x0f0f
{-# INLINE μ2 #-}

-- | Subwords of size 2 ^ 3 alternating between all bits cleared and all bits
μ3 :: Word16
μ3 = 0x00ff
{-# INLINE μ3 #-}

-- | Subwords of size 2 ^ 4 alternating between all bits cleared and all bits
μ4 :: Word16
μ4 = 0x0000
{-# INLINE μ4 #-}
