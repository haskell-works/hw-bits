{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Succinct.Internal
    ( -- * Rank & Select
      BitWise(..)
    , BitLength(..)
    , BitRank(..)
    , BitSelect(..)
    , PopCount(..)
    , Rank(..)
    , Select(..)
    , Shift(..)
    , TestBit(..)
    ) where

import qualified Data.Bits as B
import           Data.Int
import           Data.Word

infixl 9 .?.


infixl 8 .<., .>.
infixl 7 .&.
infixl 6 .^.
infixl 5 .|.

class BitLength v where
  bitLength :: v -> Int64

class BitRank v where
  bitRank :: Int64 -> v -> Int64

class BitSelect v where
  bitSelect :: Int64 -> v -> Int64

class Rank v a where
  rank :: Eq a => Int64 -> a -> v -> Int64

class Select v where
  select :: Eq a => Int64 -> v -> a -> Int64

class Shift a where
  (.<.) :: a -> Int64 -> a
  (.>.) :: a -> Int64 -> a

class TestBit a where
  (.?.) :: a -> Int64 -> Bool

class BitWise a where
  (.&.) :: a -> a -> a
  (.|.) :: a -> a -> a
  (.^.) :: a -> a -> a

class PopCount a where
  popCount :: a -> Int64

instance BitLength Word8 where
  bitLength _ = 8

instance BitLength Word16 where
  bitLength _ = 16

instance BitLength Word32 where
  bitLength _ = 32

instance BitLength Word64 where
  bitLength _ = 64

instance TestBit Word8 where
  (.?.) w n = B.testBit w (fromIntegral n)

instance TestBit Word16 where
  (.?.) w n = B.testBit w (fromIntegral n)

instance TestBit Word32 where
  (.?.) w n = B.testBit w (fromIntegral n)

instance TestBit Word64 where
  (.?.) w n = B.testBit w (fromIntegral n)

instance PopCount Word8 where
  popCount = fromIntegral . B.popCount

instance PopCount Word16 where
  popCount = fromIntegral . B.popCount

instance PopCount Word32 where
  popCount = fromIntegral . B.popCount

instance PopCount Word64 where
  popCount = fromIntegral . B.popCount

instance BitWise Word8 where
  (.&.) = (B..&.)
  (.|.) = (B..|.)
  (.^.) = B.xor

instance BitWise Word16 where
  (.&.) = (B..&.)
  (.|.) = (B..|.)
  (.^.) = B.xor

instance BitWise Word32 where
  (.&.) = (B..&.)
  (.|.) = (B..|.)
  (.^.) = B.xor

instance BitWise Word64 where
  (.&.) = (B..&.)
  (.|.) = (B..|.)
  (.^.) = B.xor

instance Shift Word8  where
  (.<.) w n = B.shiftL w (fromIntegral n)
  (.>.) w n = B.shiftR w (fromIntegral n)

instance Shift Word16 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  (.>.) w n = B.shiftR w (fromIntegral n)

instance Shift Word32 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  (.>.) w n = B.shiftR w (fromIntegral n)

instance Shift Word64 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  (.>.) w n = B.shiftR w (fromIntegral n)
