{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Bits.Broadword.Word64
  ( h
  , l
  , μ0
  , μ1
  , μ2
  , μ3
  , μ4
  , μ5
  , μ6
  , kBitDiff
  , kBitDiffPos
  , kBitDiffUnsafe
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise

-- | Initialise all sub-words of size k where 'k' ∈ { 2, 4, 8, 16, 32, 64 } such that the lowest bit is set to 1 and all other bits are cleared.
--
-- >>> import Numeric(showHex)
-- >>> showHex (l 2) ""
-- "5555555555555555"
-- >>> showHex (l 4) ""
-- "1111111111111111"
-- >>> showHex (l 8) ""
-- "101010101010101"
-- >>> showHex (l 16) ""
-- "1000100010001"
-- >>> showHex (l 32) ""
-- "100000001"
-- >>> showHex (l 64) ""
-- "1"
l :: Int -> Word64
l 2  = 0x5555555555555555
l 4  = 0x1111111111111111
l 8  = 0x0101010101010101
l 16 = 0x0001000100010001
l 32 = 0x0000000100000001
l 64 = 0x0000000000000001
l k  = error ("Invalid h k where k = " ++ show k)
{-# INLINE l #-}

-- | Initialise all sub-words of size k where 'k' ∈ { 2, 4, 8, 16, 32, 64 } such that the highest bit is set to 1 and all other bits are cleared.
--
-- >>> import Numeric(showHex)
-- >>> showHex (h 2) ""
-- "aaaaaaaaaaaaaaaa"
-- >>> showHex (h 4) ""
-- "8888888888888888"
-- >>> showHex (h 8) ""
-- "8080808080808080"
-- >>> showHex (h 16) ""
-- "8000800080008000"
-- >>> showHex (h 32) ""
-- "8000000080000000"
-- >>> showHex (h 64) ""
-- "8000000000000000"
h :: Int -> Word64
h 2  = 0xaaaaaaaaaaaaaaaa
h 4  = 0x8888888888888888
h 8  = 0x8080808080808080
h 16 = 0x8000800080008000
h 32 = 0x8000000080000000
h 64 = 0x8000000000000000
h k  = error ("Invalid h k where k = " ++ show k)
{-# INLINE h #-}

-- | Broadword subtraction of sub-words of size 'k' where 'k' ∈ { 2, 4, 8, 16, 32, 64 }.
--
-- The subtraction respects 2's complement so sub-words may be regarded as signed or unsigned words.
--
-- >>> import Numeric(showHex)
-- >>> showHex (kBitDiff 8 0x0807060504030201 0x0404030302020101) ""
-- "403030202010100"
-- >>> showHex (kBitDiff 8 0x0807060504030201 0x0102030405060708) ""
-- "7050301fffdfbf9"
-- >>> showHex (kBitDiff 8 0x20000000000000ff 0x10000000000000ff) ""
-- "1000000000000000"
kBitDiff :: Int -> Word64 -> Word64 -> Word64
kBitDiff k x y = ((x .|. h k) - (y .&. comp (h k))) .^. ((x .^. comp y) .&. h k)
{-# INLINE kBitDiff #-}

-- | Broadword subtraction of sub-words of size 'k' where 'k' ∈ { 2, 4, 8, 16, 32, 64 } where results are bounded from below by 0.
--
-- >>> import Numeric(showHex)
-- >>> showHex (kBitDiffPos 8 0x0807060504030201 0x0404030302020101) ""
-- "403030202010100"
-- >>> showHex (kBitDiffPos 8 0x0807060504030201 0x0102030405060708) ""
-- "705030100000000"
-- >>> showHex (kBitDiffPos 8 0x20000000000000ff 0x10000000000000ff) ""
-- "1000000000000000"
kBitDiffPos :: Int -> Word64 -> Word64 -> Word64
kBitDiffPos k x y = let d = kBitDiff k x y in d .&. kBitDiff k 0 ((comp d .&. h 8) .>. fromIntegral (k - 1))
{-# INLINE kBitDiffPos #-}

-- | Broadword subtraction of sub-words of size 'k' where 'k' ∈ { 2, 4, 8, 16, 32, 64 } where all the sub-words of 'x' and 'y' must
-- not have the signed bit set for the result to be meaningful.
--
-- >>> import Numeric(showHex)
-- >>> showHex (kBitDiffUnsafe 8 0x0807060504030201 0x0404030302020101) ""
-- "403030202010100"
-- >>> showHex (kBitDiffUnsafe 8 0x0807060504030201 0x0102030405060708) ""
-- "7050301fffdfbf9"
-- >>> showHex (kBitDiffUnsafe 8 0x20000000000000ff 0x10000000000000ff) "" -- produces nonsense in the last sub-word
-- "1000000000000080"
kBitDiffUnsafe :: Int -> Word64 -> Word64 -> Word64
kBitDiffUnsafe k x y = ((x .|. h k) - y) .^. h k
{-# INLINE kBitDiffUnsafe #-}

-- | Subwords of size 2 ^ 0 alternating between all bits cleared and all bits
μ0 :: Word64
μ0 = 0x5555555555555555
{-# INLINE μ0 #-}

-- | Subwords of size 2 ^ 1 alternating between all bits cleared and all bits
μ1 :: Word64
μ1 = 0x3333333333333333
{-# INLINE μ1 #-}

-- | Subwords of size 2 ^ 2 alternating between all bits cleared and all bits
μ2 :: Word64
μ2 = 0x0f0f0f0f0f0f0f0f
{-# INLINE μ2 #-}

-- | Subwords of size 2 ^ 3 alternating between all bits cleared and all bits
μ3 :: Word64
μ3 = 0x00ff00ff00ff00ff
{-# INLINE μ3 #-}

-- | Subwords of size 2 ^ 4 alternating between all bits cleared and all bits
μ4 :: Word64
μ4 = 0x0000ffff0000ffff
{-# INLINE μ4 #-}

-- | Subwords of size 2 ^ 5 alternating between all bits cleared and all bits
μ5 :: Word64
μ5 = 0x00000000ffffffff
{-# INLINE μ5 #-}

-- | Subwords of size 2 ^ 6 alternating between all bits cleared and all bits
μ6 :: Word64
μ6 = 0xffffffffffffffff
{-# INLINE μ6 #-}
