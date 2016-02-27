{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE TypeFamilies           #-}

module HaskellWorks.Data.Bits where

import Data.Word
import HaskellWorks.Data.Succinct.BitWise

class BitConcat a where
  type DoubleBits a
  leConcat :: a -> a -> DoubleBits a

class BitSplit a where
  type HalfBits a
  leSplit :: a -> (HalfBits a, HalfBits a)

instance BitConcat Word8 where
  type DoubleBits Word8 = Word16
  leConcat a b = (fromIntegral b .<. bitLength a) .|. fromIntegral a

instance BitConcat Word16 where
  type DoubleBits Word16 = Word32
  leConcat a b = (fromIntegral b .<. bitLength a) .|. fromIntegral a

instance BitConcat Word32 where
  type DoubleBits Word32 = Word64
  leConcat a b = (fromIntegral b .<. bitLength a) .|. fromIntegral a

instance BitSplit Word64 where
  type HalfBits Word64 = Word32
  leSplit a = (fromIntegral a, fromIntegral (a .>. 32))

instance BitSplit Word32 where
  type HalfBits Word32 = Word16
  leSplit a = (fromIntegral a, fromIntegral (a .>. 16))

instance BitSplit Word16 where
  type HalfBits Word16 = Word8
  leSplit a = (fromIntegral a, fromIntegral (a .>. 8))
