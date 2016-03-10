{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Vector.VectorLike
  ( VectorLike(..)
  ) where

import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.Positioning

class VectorLike v e where
  toList :: v e -> [e]
  fromList :: [e] -> v e
  (!!!) :: v e -> Position -> e
  concat :: [v e] -> v e
  empty :: v e
  filter :: (e -> Bool) -> v e -> v e
  generate :: Int -> (Int -> e) -> v e
  length :: v e -> Count
  snoc :: v e -> e -> v e
  sum :: v e -> e
  unsafeIndex :: v e -> Position -> e
  unsafeSlice :: Position -> Position -> v e -> v e

instance VectorLike DV.Vector Word8 where
  toList = DV.toList
  {-# INLINABLE toList #-}

  fromList = DV.fromList
  {-# INLINABLE fromList #-}

  (!!!) v (Position i) = v DV.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  concat = DV.concat
  {-# INLINABLE concat #-}

  empty = DV.empty
  {-# INLINABLE empty #-}

  filter = DV.filter
  {-# INLINABLE filter #-}

  generate = DV.generate
  {-# INLINABLE generate #-}

  length = Count . fromIntegral . DV.length
  {-# INLINABLE length #-}

  snoc = DV.snoc
  {-# INLINABLE snoc #-}

  sum = DV.sum
  {-# INLINABLE sum #-}

  unsafeIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}

instance VectorLike DV.Vector Word16 where
  toList = DV.toList
  {-# INLINABLE toList #-}

  fromList = DV.fromList
  {-# INLINABLE fromList #-}

  (!!!) v (Position i) = v DV.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  concat = DV.concat
  {-# INLINABLE concat #-}

  empty = DV.empty
  {-# INLINABLE empty #-}

  filter = DV.filter
  {-# INLINABLE filter #-}

  generate = DV.generate
  {-# INLINABLE generate #-}

  length = Count . fromIntegral . DV.length
  {-# INLINABLE length #-}

  snoc = DV.snoc
  {-# INLINABLE snoc #-}

  sum = DV.sum
  {-# INLINABLE sum #-}

  unsafeIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}

instance VectorLike DV.Vector Word32 where
  toList = DV.toList
  {-# INLINABLE toList #-}

  fromList = DV.fromList
  {-# INLINABLE fromList #-}

  (!!!) v (Position i) = v DV.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  concat = DV.concat
  {-# INLINABLE concat #-}

  empty = DV.empty
  {-# INLINABLE empty #-}

  filter = DV.filter
  {-# INLINABLE filter #-}

  generate = DV.generate
  {-# INLINABLE generate #-}

  length = Count . fromIntegral . DV.length
  {-# INLINABLE length #-}

  snoc = DV.snoc
  {-# INLINABLE snoc #-}

  sum = DV.sum
  {-# INLINABLE sum #-}

  unsafeIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}

instance VectorLike DV.Vector Word64 where
  toList = DV.toList
  {-# INLINABLE toList #-}

  fromList = DV.fromList
  {-# INLINABLE fromList #-}

  (!!!) v (Position i) = v DV.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  concat = DV.concat
  {-# INLINABLE concat #-}

  empty = DV.empty
  {-# INLINABLE empty #-}

  filter = DV.filter
  {-# INLINABLE filter #-}

  generate = DV.generate
  {-# INLINABLE generate #-}

  length = Count . fromIntegral . DV.length
  {-# INLINABLE length #-}

  snoc = DV.snoc
  {-# INLINABLE snoc #-}

  sum = DV.sum
  {-# INLINABLE sum #-}

  unsafeIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}

instance VectorLike DVS.Vector Word8 where
  toList = DVS.toList
  {-# INLINABLE toList #-}

  fromList = DVS.fromList
  {-# INLINABLE fromList #-}

  (!!!) v (Position i) = v DVS.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  concat = DVS.concat
  {-# INLINABLE concat #-}

  empty = DVS.empty
  {-# INLINABLE empty #-}

  filter = DVS.filter
  {-# INLINABLE filter #-}

  generate = DVS.generate
  {-# INLINABLE generate #-}

  length = Count . fromIntegral . DVS.length
  {-# INLINABLE length #-}

  snoc = DVS.snoc
  {-# INLINABLE snoc #-}

  sum = DVS.sum
  {-# INLINABLE sum #-}

  unsafeIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}

instance VectorLike DVS.Vector Word16 where
  toList = DVS.toList
  {-# INLINABLE toList #-}

  fromList = DVS.fromList
  {-# INLINABLE fromList #-}

  (!!!) v (Position i) = v DVS.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  concat = DVS.concat
  {-# INLINABLE concat #-}

  empty = DVS.empty
  {-# INLINABLE empty #-}

  filter = DVS.filter
  {-# INLINABLE filter #-}

  generate = DVS.generate
  {-# INLINABLE generate #-}

  length = Count . fromIntegral . DVS.length
  {-# INLINABLE length #-}

  snoc = DVS.snoc
  {-# INLINABLE snoc #-}

  sum = DVS.sum
  {-# INLINABLE sum #-}

  unsafeIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}

instance VectorLike DVS.Vector Word32 where
  toList = DVS.toList
  {-# INLINABLE toList #-}

  fromList = DVS.fromList
  {-# INLINABLE fromList #-}

  (!!!) v (Position i) = v DVS.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  concat = DVS.concat
  {-# INLINABLE concat #-}

  empty = DVS.empty
  {-# INLINABLE empty #-}

  filter = DVS.filter
  {-# INLINABLE filter #-}

  generate = DVS.generate
  {-# INLINABLE generate #-}

  length = Count . fromIntegral . DVS.length
  {-# INLINABLE length #-}

  snoc = DVS.snoc
  {-# INLINABLE snoc #-}

  sum = DVS.sum
  {-# INLINABLE sum #-}

  unsafeIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}

instance VectorLike DVS.Vector Word64 where
  toList = DVS.toList
  {-# INLINABLE toList #-}

  fromList = DVS.fromList
  {-# INLINABLE fromList #-}

  (!!!) v (Position i) = v DVS.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  concat = DVS.concat
  {-# INLINABLE concat #-}

  empty = DVS.empty
  {-# INLINABLE empty #-}

  filter = DVS.filter
  {-# INLINABLE filter #-}

  generate = DVS.generate
  {-# INLINABLE generate #-}

  length = Count . fromIntegral . DVS.length
  {-# INLINABLE length #-}

  snoc = DVS.snoc
  {-# INLINABLE snoc #-}

  sum = DVS.sum
  {-# INLINABLE sum #-}

  unsafeIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}
