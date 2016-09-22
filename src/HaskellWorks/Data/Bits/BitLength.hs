{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.BitLength
    ( -- * Bit map
      BitLength(..)
    , elemBitLength
    , elemBitEnd
    ) where

import qualified Data.Vector                         as DV
import qualified Data.Vector.Storable                as DVS
import           Data.Word
import           HaskellWorks.Data.IndexedSeq
import           HaskellWorks.Data.Naive
import           HaskellWorks.Data.Positioning
import           Prelude                             as P

-- | Number of bits in a value including ones and zeros.
class BitLength v where
  bitLength :: v -> Count

  endPosition :: v -> Position
  endPosition = Position . fromIntegral . getCount . bitLength
  {-# INLINE endPosition #-}

--------------------------------------------------------------------------------
-- Functions

elemBitLength :: (IndexedSeq v, BitLength (Elem v)) => v -> Count
elemBitLength v = bitLength (v !!! 0)
{-# INLINE elemBitLength #-}

elemBitEnd :: (IndexedSeq v, BitLength (Elem v)) => v -> Position
elemBitEnd v = endPosition (v !!! 0)
{-# INLINE elemBitEnd #-}

--------------------------------------------------------------------------------
-- Instances

instance BitLength Bool where
  bitLength _ = 1
  {-# INLINE bitLength #-}

instance BitLength [Bool] where
  bitLength = fromIntegral . P.length
  {-# INLINE bitLength #-}

instance BitLength Word8 where
  bitLength _ = 8
  {-# INLINE bitLength #-}

instance BitLength Word16 where
  bitLength _ = 16
  {-# INLINE bitLength #-}

instance BitLength Word32 where
  bitLength _ = 32
  {-# INLINE bitLength #-}

instance BitLength Word64 where
  bitLength _ = 64
  {-# INLINE bitLength #-}

instance BitLength (Naive Word8) where
  bitLength _ = 8
  {-# INLINE bitLength #-}

instance BitLength (Naive Word16) where
  bitLength _ = 16
  {-# INLINE bitLength #-}

instance BitLength (Naive Word32) where
  bitLength _ = 32
  {-# INLINE bitLength #-}

instance BitLength (Naive Word64) where
  bitLength _ = 64
  {-# INLINE bitLength #-}

instance BitLength [Word8] where
  bitLength v = fromIntegral (P.length v) * bitLength (head v)
  {-# INLINE bitLength #-}

instance BitLength [Word16] where
  bitLength v = fromIntegral (P.length v) * bitLength (head v)
  {-# INLINE bitLength #-}

instance BitLength [Word32] where
  bitLength v = fromIntegral (P.length v) * bitLength (head v)
  {-# INLINE bitLength #-}

instance BitLength [Word64] where
  bitLength v = fromIntegral (P.length v) * bitLength (head v)
  {-# INLINE bitLength #-}

instance BitLength (DV.Vector Word8) where
  bitLength v = vLength v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}

instance BitLength (DV.Vector Word16) where
  bitLength v = vLength v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}

instance BitLength (DV.Vector Word32) where
  bitLength v = vLength v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}

instance BitLength (DV.Vector Word64) where
  bitLength v = vLength v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}

instance BitLength (DVS.Vector Word8) where
  bitLength v = vLength v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}

instance BitLength (DVS.Vector Word16) where
  bitLength v = vLength v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}

instance BitLength (DVS.Vector Word32) where
  bitLength v = vLength v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}

instance BitLength (DVS.Vector Word64) where
  bitLength v = vLength v * bitLength (v !!! 0)
  {-# INLINE bitLength #-}
