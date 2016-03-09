{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.PopCount.PopCount0
    ( PopCount0(..)
    ) where

import qualified Data.Vector                               as DV
import qualified Data.Vector.Storable                      as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.PopCount.PopCount1
import           HaskellWorks.Data.Positioning
import           Prelude                                   as P

class PopCount0 v where
  popCount0 :: v -> Count

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

instance PopCount0 (DV.Vector Word8) where
  popCount0 = DV.foldl (\c -> (c +) . popCount0) 0
  {-# INLINABLE popCount0 #-}

instance PopCount0 (DV.Vector Word16) where
  popCount0 = DV.foldl (\c -> (c +) . popCount0) 0
  {-# INLINABLE popCount0 #-}

instance PopCount0 (DV.Vector Word32) where
  popCount0 = DV.foldl (\c -> (c +) . popCount0) 0
  {-# INLINABLE popCount0 #-}

instance PopCount0 (DV.Vector Word64) where
  popCount0 = DV.foldl (\c -> (c +) . popCount0) 0
  {-# INLINABLE popCount0 #-}

instance PopCount0 (DVS.Vector Word8) where
  popCount0 = DVS.foldl (\c -> (c +) . popCount0) 0
  {-# INLINABLE popCount0 #-}

instance PopCount0 (DVS.Vector Word16) where
  popCount0 = DVS.foldl (\c -> (c +) . popCount0) 0
  {-# INLINABLE popCount0 #-}

instance PopCount0 (DVS.Vector Word32) where
  popCount0 = DVS.foldl (\c -> (c +) . popCount0) 0
  {-# INLINABLE popCount0 #-}

instance PopCount0 (DVS.Vector Word64) where
  popCount0 = DVS.foldl (\c -> (c +) . popCount0) 0
  {-# INLINABLE popCount0 #-}
