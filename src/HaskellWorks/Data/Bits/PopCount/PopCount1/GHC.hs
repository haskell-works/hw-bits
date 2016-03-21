{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.PopCount.PopCount1.GHC
    ( PopCount1(..)
    ) where

import qualified Data.Bits                     as DB
import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.Positioning
import           Prelude                       as P

class PopCount1 v where
  popCount1 :: v -> Count

instance PopCount1 Bool where
  popCount1 True  = 1
  popCount1 False = 0
  {-# INLINABLE popCount1 #-}

instance PopCount1 Word8 where
  popCount1 = fromIntegral . DB.popCount
  {-# INLINABLE popCount1 #-}

instance PopCount1 Word16 where
  popCount1 = fromIntegral . DB.popCount
  {-# INLINABLE popCount1 #-}

instance PopCount1 Word32 where
  popCount1 = fromIntegral . DB.popCount
  {-# INLINABLE popCount1 #-}

instance PopCount1 Word64 where
  popCount1 = fromIntegral . DB.popCount
  {-# INLINABLE popCount1 #-}

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

instance PopCount1 (DV.Vector Word8) where
  popCount1 = DV.foldl (\c -> (c +) . popCount1) 0
  {-# INLINABLE popCount1 #-}

instance PopCount1 (DV.Vector Word16) where
  popCount1 = DV.foldl (\c -> (c +) . popCount1) 0
  {-# INLINABLE popCount1 #-}

instance PopCount1 (DV.Vector Word32) where
  popCount1 = DV.foldl (\c -> (c +) . popCount1) 0
  {-# INLINABLE popCount1 #-}

instance PopCount1 (DV.Vector Word64) where
  popCount1 = DV.foldl (\c -> (c +) . popCount1) 0
  {-# INLINABLE popCount1 #-}

instance PopCount1 (DVS.Vector Word8) where
  popCount1 = DVS.foldl (\c -> (c +) . popCount1) 0
  {-# INLINABLE popCount1 #-}

instance PopCount1 (DVS.Vector Word16) where
  popCount1 = DVS.foldl (\c -> (c +) . popCount1) 0
  {-# INLINABLE popCount1 #-}

instance PopCount1 (DVS.Vector Word32) where
  popCount1 = DVS.foldl (\c -> (c +) . popCount1) 0
  {-# INLINABLE popCount1 #-}

instance PopCount1 (DVS.Vector Word64) where
  popCount1 = DVS.foldl (\c -> (c +) . popCount1) 0
  {-# INLINABLE popCount1 #-}
