{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.BitWise
    ( -- * Bit map
      BitLength(..)
    , BitWise(..)
    , PopCount(..)
    , PopCount0(..)
    , PopCount1(..)
    , Shift(..)
    , TestBit(..)
    , elemBitLength
    , elemBitEnd
    ) where

import qualified Data.Bits                     as B
import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.VectorLike  as VL
import           Prelude                       as P

-- We pervasively use precedence to avoid excessive parentheses, and we use
-- the same precedence conventions of the C programming language: arithmetic
-- operators come first, ordered in the standard way, followed by shifts,
-- followed by logical operators; ⊕ sits between | and &.
infixl 9 .?.
infixl 8 .<., .>.
infixl 7 .&.        -- Bitwise AND.  eg. ∧
infixl 6 .^.        -- Bitwise XOR.  eg. ⊕
infixl 5 .|.        -- Bitwise OR.   eg. ∨

class BitLength v where
  bitLength :: v -> Count

  endPosition :: v -> Position
  endPosition = Position . fromIntegral . getCount . bitLength

class Shift a where
  (.<.) :: a -> Count -> a
  (.>.) :: a -> Count -> a

class TestBit a where
  (.?.) :: a -> Position -> Bool

class BitWise a where
  (.&.) :: a -> a -> a
  (.|.) :: a -> a -> a
  (.^.) :: a -> a -> a
  comp  :: a -> a
  all0s :: a
  all1s :: a

class PopCount v e where
  popCount :: e -> v -> Count

class PopCount0 v where
  popCount0 :: v -> Count

class PopCount1 v where
  popCount1 :: v -> Count

--------------------------------------------------------------------------------
-- Functions

elemBitLength :: (VectorLike v e, BitLength e) => v e -> Count
elemBitLength v = bitLength (v !!! 0)

elemBitEnd :: (VectorLike v e, BitLength e) => v e -> Position
elemBitEnd v = endPosition (v !!! 0)

--------------------------------------------------------------------------------
-- Instances

instance BitLength Bool where
  bitLength _ = 1
  {-# INLINABLE bitLength #-}

instance BitLength [Bool] where
  bitLength = fromIntegral . P.length
  {-# INLINABLE bitLength #-}

instance BitLength Word8 where
  bitLength _ = 8
  {-# INLINABLE bitLength #-}

instance BitLength Word16 where
  bitLength _ = 16
  {-# INLINABLE bitLength #-}

instance BitLength Word32 where
  bitLength _ = 32
  {-# INLINABLE bitLength #-}

instance BitLength Word64 where
  bitLength _ = 64
  {-# INLINABLE bitLength #-}

instance BitLength [Word8] where
  bitLength v = fromIntegral (P.length v) * bitLength (head v)
  {-# INLINABLE bitLength #-}

instance BitLength [Word16] where
  bitLength v = fromIntegral (P.length v) * bitLength (head v)
  {-# INLINABLE bitLength #-}

instance BitLength [Word32] where
  bitLength v = fromIntegral (P.length v) * bitLength (head v)
  {-# INLINABLE bitLength #-}

instance BitLength [Word64] where
  bitLength v = fromIntegral (P.length v) * bitLength (head v)
  {-# INLINABLE bitLength #-}

instance BitLength (DV.Vector Word8) where
  bitLength v = VL.length v * bitLength (v !!! 0)
  {-# INLINABLE bitLength #-}

instance BitLength (DV.Vector Word16) where
  bitLength v = VL.length v * bitLength (v !!! 0)
  {-# INLINABLE bitLength #-}

instance BitLength (DV.Vector Word32) where
  bitLength v = VL.length v * bitLength (v !!! 0)
  {-# INLINABLE bitLength #-}

instance BitLength (DV.Vector Word64) where
  bitLength v = VL.length v * bitLength (v !!! 0)
  {-# INLINABLE bitLength #-}

instance BitLength (DVS.Vector Word8) where
  bitLength v = VL.length v * bitLength (v !!! 0)
  {-# INLINABLE bitLength #-}

instance BitLength (DVS.Vector Word16) where
  bitLength v = VL.length v * bitLength (v !!! 0)
  {-# INLINABLE bitLength #-}

instance BitLength (DVS.Vector Word32) where
  bitLength v = VL.length v * bitLength (v !!! 0)
  {-# INLINABLE bitLength #-}

instance BitLength (DVS.Vector Word64) where
  bitLength v = VL.length v * bitLength (v !!! 0)
  {-# INLINABLE bitLength #-}

instance TestBit Bool where
  (.?.) w 0 = w
  (.?.) _ _ = error "Invalid bit index"
  {-# INLINABLE (.?.) #-}

instance TestBit [Bool] where
  (.?.) v p = v !! fromIntegral p
  {-# INLINABLE (.?.) #-}

instance TestBit Word8 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance TestBit Word16 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance TestBit Word32 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance TestBit Word64 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance TestBit (DV.Vector Word8) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance TestBit (DV.Vector Word16) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance TestBit (DV.Vector Word32) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance TestBit (DV.Vector Word64) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance TestBit (DVS.Vector Word8) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance TestBit (DVS.Vector Word16) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance TestBit (DVS.Vector Word32) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance TestBit (DVS.Vector Word64) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance PopCount1 Bool where
  popCount1 True  = 1
  popCount1 False = 0
  {-# INLINABLE popCount1 #-}

instance PopCount1 Word8 where
  popCount1 x0 = Count (fromIntegral x3)
    where
      x1 = x0 - ((x0 .&. 0xaa) .>. 1)
      x2 = (x1 .&. 0x33) + ((x1 .>. 2) .&. 0x33)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f
  {-# INLINABLE popCount1 #-}

instance PopCount1 Word16 where
  popCount1 x0 = Count (fromIntegral ((x3 * 0x0101) .>. 8))
    where
      x1 = x0 - ((x0 .&. 0xaaaa) .>. 1)
      x2 = (x1 .&. 0x3333) + ((x1 .>. 2) .&. 0x3333)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f0f
  {-# INLINABLE popCount1 #-}

instance PopCount1 Word32 where
  popCount1 x0 = Count (fromIntegral ((x3 * 0x01010101) .>. 24))
    where
      x1 = x0 - ((x0 .&. 0xaaaaaaaa) .>. 1)
      x2 = (x1 .&. 0x33333333) + ((x1 .>. 2) .&. 0x33333333)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f0f0f0f
  {-# INLINABLE popCount1 #-}

instance PopCount1 Word64 where
  popCount1 x0 = Count ((x3 * 0x0101010101010101) .>. 56)
    where
      x1 = x0 - ((x0 .&. 0xaaaaaaaaaaaaaaaa) .>. 1)
      x2 = (x1 .&. 0x3333333333333333) + ((x1 .>. 2) .&. 0x3333333333333333)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f0f0f0f0f0f0f0f
  {-# INLINABLE popCount1 #-}

instance PopCount0 Bool where
  popCount0 True  = 0
  popCount0 False = 1
  {-# INLINABLE popCount0 #-}

instance PopCount0 Word8 where
  popCount0 x0 = bitLength x0 - popCount1 x0
  {-# INLINABLE popCount0 #-}

instance PopCount0 Word16 where
  popCount0 x0 = bitLength x0 - popCount1 x0
  {-# INLINABLE popCount0 #-}

instance PopCount0 Word32 where
  popCount0 x0 = bitLength x0 - popCount1 x0
  {-# INLINABLE popCount0 #-}

instance PopCount0 Word64 where
  popCount0 x0 = bitLength x0 - popCount1 x0
  {-# INLINABLE popCount0 #-}

instance PopCount1 [Bool] where
  popCount1 = P.sum . fmap popCount1
  {-# INLINABLE popCount1 #-}

instance PopCount1 [Word8] where
  popCount1 = P.sum . fmap popCount1
  {-# INLINABLE popCount1 #-}

instance PopCount1 [Word16] where
  popCount1 = P.sum . fmap popCount1
  {-# INLINABLE popCount1 #-}

instance PopCount1 [Word32] where
  popCount1 = P.sum . fmap popCount1
  {-# INLINABLE popCount1 #-}

instance PopCount1 [Word64] where
  popCount1 = P.sum . fmap popCount1
  {-# INLINABLE popCount1 #-}

instance PopCount0 [Bool] where
  popCount0 = P.sum . fmap popCount0
  {-# INLINABLE popCount0 #-}

instance PopCount0 [Word8] where
  popCount0 = P.sum . fmap popCount0
  {-# INLINABLE popCount0 #-}

instance PopCount0 [Word16] where
  popCount0 = P.sum . fmap popCount0
  {-# INLINABLE popCount0 #-}

instance PopCount0 [Word32] where
  popCount0 = P.sum . fmap popCount0
  {-# INLINABLE popCount0 #-}

instance PopCount0 [Word64] where
  popCount0 = P.sum . fmap popCount0
  {-# INLINABLE popCount0 #-}

instance BitWise Word8 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

  all0s = 0
  {-# INLINABLE all0s #-}

  all1s = 0
  {-# INLINABLE all1s #-}

instance BitWise Word16 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

  all0s = 0
  {-# INLINABLE all0s #-}

  all1s = 0
  {-# INLINABLE all1s #-}

instance BitWise Word32 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

  all0s = 0
  {-# INLINABLE all0s #-}

  all1s = 0
  {-# INLINABLE all1s #-}

instance BitWise Word64 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

  all0s = 0
  {-# INLINABLE all0s #-}

  all1s = 0
  {-# INLINABLE all1s #-}

instance Shift Word8  where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}

instance Shift Word16 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}

instance Shift Word32 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}

instance Shift Word64 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}
