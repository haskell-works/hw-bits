{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.Broadword
  ( Broadword(..)
  , broadword
  , h
  , l
  , lsb
  , kBitDiff
  , kBitDiffSimple
  ) where

import qualified Data.Bits                        as DB
import           Data.Word
import           HaskellWorks.Data.Bits.BitWise

newtype Broadword a = Broadword a deriving (Eq, Show)

broadword :: Broadword Word64 -> Word64
broadword (Broadword a) = a
{-# INLINE broadword #-}

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

kBitDiff :: Int -> Word64 -> Word64 -> Word64
kBitDiff k x y = ((x .|. h k) - (y .&. comp (h k))) .^. ((x .^. comp y) .&. h k)
{-# INLINE kBitDiff #-}

kBitDiffSimple :: Int -> Word64 -> Word64 -> Word64
kBitDiffSimple k x y = ((x .|. h k) - y) .^. h k
{-# INLINE kBitDiffSimple #-}

lsb :: Word64 -> Word64
lsb w = let r = fromIntegral (DB.countTrailingZeros w) in r .|. negate ((r .>. 6) .&. 0x1)
{-# INLINE lsb #-}
