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
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Vector.VectorLike as VL
import           Prelude                             as P

class BitLength v where
  bitLength :: v -> Count

  endPosition :: v -> Position
  endPosition = Position . fromIntegral . getCount . bitLength

--------------------------------------------------------------------------------
-- Functions

elemBitLength :: (VectorLike v, BitLength (Elem v)) => v -> Count
elemBitLength v = bitLength (v !!! 0)

elemBitEnd :: (VectorLike v, BitLength (Elem v)) => v -> Position
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
