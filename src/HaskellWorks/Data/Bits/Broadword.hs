{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE BangPatterns          #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.Broadword
  ( h
  , l
  , kBitDiff
  , kBitDiffSimple
  , findCloseW64
  ) where

import qualified Data.Bits                        as DB
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Bits.BitWise
import           Debug.Trace

l :: Int -> Word64
l 2   = 0x5555555555555555
l 4   = 0x1111111111111111
l 8   = 0x0101010101010101
l 16  = 0x0001000100010001
l 32  = 0x0000000100000001
l 64  = 0x0000000000000001
l k   = error ("Invalid h k where k = " ++ show k)
{-# INLINE l #-}

h :: Int -> Word64
h 2   = 0xaaaaaaaaaaaaaaaa
h 4   = 0x8888888888888888
h 8   = 0x8080808080808080
h 16  = 0x8000800080008000
h 32  = 0x8000000080000000
h 64  = 0x8000000000000000
h k   = error ("Invalid h k where k = " ++ show k)
{-# INLINE h #-}

-- Section 3 Eq 1
kBitDiff :: Int -> Word64 -> Word64 -> Word64
kBitDiff k x y = ((x .|. h k) - (y .&. comp (h k))) .^. ((x .^. comp y) .&. h k)
{-# INLINE kBitDiff #-}

-- Section 3 Eq 2
kBitDiffSimple :: Int -> Word64 -> Word64 -> Word64
kBitDiffSimple k x y = ((x .|. h k) - y) .^. h k
{-# INLINE kBitDiffSimple #-}

traceW :: String -> Word64 -> Word64
traceW s w = trace (s ++ ": " ++ show (BitShown w) ++ " : " ++ show w) w

lsb :: Word64 -> Word64
lsb w = fromIntegral (DB.countTrailingZeros w)

findCloseW64 :: Word64 -> Word64
findCloseW64 x = let !_ = traceW "x00" x in
  let !b00 = x - ((x .&. 0xaaaaaaaaaaaaaaaa) .>. 1)                                 in let !_ = traceW "b00" b00 in
  let !b01 = (b00 .&. 0x3333333333333333) + ((b00 .>. 2) .&. 0x3333333333333333)    in let !_ = traceW "b01" b01 in
  let !b02 = (b01 + (b01 .>. 4)) .&. 0x0f0f0f0f0f0f0f0f                             in let !_ = traceW "b02" b02 in
  let !b03 = (b02 * 0x0101010101010101) .<. 1                                       in let !_ = traceW "b03" b03 in
  let !b04 = kBitDiff 8 (h 8 .|. 0x4038302820181008) b03                            in let !_ = traceW "b04" b04 in
  let !u00 = (((((b04 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                in let !_ = traceW "u00" u00 in
  let !z00 = ((h 8 .>. 1) .|. (l 8 * 7)) .&. u00                                    in let !_ = traceW "z00" z00 in
  let !b10 = b04 - (l 8 * 2 - ((x .>. 6 .&. l 8 .<. 1) + (x .>. 5 .&. l 8 .<. 1)))  in let !_ = traceW "b10" b10 in
  let !u10 = (((((b10 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                in let !_ = traceW "u10" u10 in
  let !z10 = (z00 .&. comp u10) .|. (((h 8 .>. 1) .|. (l 8 * 5)) .&. u10)           in let !_ = traceW "z10" z10 in
  let !b20 = b10 - (l 8 * 2 - ((x .>. 4 .&. l 8 .<. 1) + (x .>. 3 .&. l 8 .<. 1)))  in let !_ = traceW "b20" b20 in
  let !u20 = (((((b20 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                in let !_ = traceW "u20" u20 in
  let !z20 = (z10 .&. comp u20) .|. (((h 8 .>. 1) .|. (l 8 * 3)) .&. u20)           in let !_ = traceW "z20" z20 in
  let !b30 = b10 - (l 8 * 2 - ((x .>. 2 .&. l 8 .<. 1) + (x .>. 1 .&. l 8 .<. 1)))  in let !_ = traceW "b30" b30 in
  let !u30 = (((((b30 .|. h 8) - l 8) .>. 7) .&. l 8) .|. h 8) - l 8                in let !_ = traceW "u30" u30 in
  let !z30 = (z20 .&. comp u30) .|. (((h 8 .>. 1) .|.  l 8     ) .&. u30)            in let !_ = traceW "z30" z30 in
  let !p00 = lsb (z30 .>. 6 .&. l 8)                                                in
  let !(r00 :: Word64) = ((p00 + (((z30 :: Word64) .>. fromIntegral p00) .&. 0x3f)) .|. (p00 .>. 8)) .&. 0x7f in let !_ = traceW "r00" r00 in
  r00
