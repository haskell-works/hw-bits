
-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Succinct.Experimental (
      broadwordPopCount
    , lte8
    , gt8
    , select9imp
    ) where

import Data.Int
import Data.Word
import HaskellWorks.Data.Succinct.Internal

select9imp :: Integral a => a -> Word64 -> Int64
select9imp r v =
  let r0 = fromIntegral r :: Word64 in
  let s0 = v - ((v .&. 0xAAAAAAAAAAAAAAAA) .>. 1) in
  let s1 = (s0 .&. 0x3333333333333333) + ((v .>. 2) .&. 0x3333333333333333) in
  let s2 = ((s1 + (s1 .>. 4)) .&. 0x0F0F0F0F0F0F0F0F0) * bwL8 in
  let b  = ((s2 `lte8` (r0 * bwL8)) .>. 7) * bwL8.>. 53 .&. 7 in
  let l  = r0 - (((s2 .<. 8) .>. fromIntegral b) .&. 0xFF) in
  let s  = (((v.>. fromIntegral b .&. 0xFF) * bwL8 .&. 0x8040201008040201 `gt8` 0) .>. 7) * bwL8 in
  let z = b + (((s `lte8` l * bwL8).>. 7) * bwL8 .>. 56) in
  fromIntegral z :: Int64

lte8 :: (Broadword a, BitWise a, Num a) => a -> a -> a
lte8 x y = ((y .|. bwH8) - (x .&. comp bwH8)) .|. x .^. y .^. (x .&. comp y) .&. bwH8

gt8 :: (Broadword a, BitWise a, Num a) => a -> a -> a
gt8 x y = ((x .|. bwH8) - bwL8) .|. x .&. bwH8

broadwordPopCount :: Word64 -> Int64
broadwordPopCount w =
    let x = w - (w .&. 0xAAAAAAAAAAAAAAAA) .>. 1 in
    let y = (x .&. 0x3333333333333333) + ((x .>. 2) .&. 0x3333333333333333) in
    let z = (y + (y .>. 4)) .&. 0x0F0F0F0F0F0F0F0F0 in
    fromIntegral (z * bwL8 .>. 56) :: Int64
