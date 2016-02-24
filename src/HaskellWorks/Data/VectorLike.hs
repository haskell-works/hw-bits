{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.VectorLike
  ( VectorLike(..)
  , elemBitLength
  , elemEndPosition
  ) where

import qualified Data.Vector                            as DV
import qualified Data.Vector.Storable                   as DVS
import           Data.Word
import           Foreign.Storable
import           HaskellWorks.Data.Succinct.BitWise
import           HaskellWorks.Data.Succinct.Positioning

class VectorLike v e where
  toList :: v e -> [e]
  fromList :: [e] -> v e
  (!!!) :: v e -> Int -> e
  concat :: [v e] -> v e
  empty :: v e
  filter :: (e -> Bool) -> v e -> v e
  generate :: Int -> (Int -> e) -> v e
  length :: v e -> Int
  snoc :: v e -> e -> v e
  sum :: v e -> e
  unsafeIndex :: v e -> Int -> e
  unsafeSlice :: Int -> Int -> v e -> v e

class BoxedVectorLike v e where
  bImap :: (Int -> a -> b) -> v a -> v b
  bMap :: (a -> b) -> v a -> v b
  bUnfoldr :: (Storable a) => (b -> Maybe (a, b)) -> b -> v a
  bUnfoldrN :: (Storable a) => Int -> (b -> Maybe (a, b)) -> b -> v a

class StorableVectorLike v e where
  sImap :: (Storable a, Storable b) => (Int -> a -> b) -> v a -> v b
  sMap :: (Storable a, Storable b) => (a -> b) -> v a -> v b
  sUnfoldr :: (Storable a) => (b -> Maybe (a, b)) -> b -> v a
  sUnfoldrN :: (Storable a) => Int -> (b -> Maybe (a, b)) -> b -> v a

elemBitLength :: (VectorLike v e, BitLength e) => v e -> Count
elemBitLength v = bitLength (v !!! 0)

elemEndPosition :: (VectorLike v e, BitLength e) => v e -> Position
elemEndPosition v = endPosition (v !!! 0)

instance VectorLike DV.Vector Word8 where
  toList = DV.toList
  {-# INLINABLE toList #-}

  fromList = DV.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DV.! i
  {-# INLINABLE (!!!) #-}

  concat = DV.concat
  {-# INLINABLE concat #-}

  empty = DV.empty
  {-# INLINABLE empty #-}

  filter = DV.filter
  {-# INLINABLE filter #-}

  generate = DV.generate
  {-# INLINABLE generate #-}

  length = DV.length
  {-# INLINABLE length #-}

  snoc = DV.snoc
  {-# INLINABLE snoc #-}

  sum = DV.sum
  {-# INLINABLE sum #-}

  unsafeIndex = DV.unsafeIndex
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice = DV.unsafeSlice
  {-# INLINABLE unsafeSlice #-}

instance BoxedVectorLike DV.Vector Word8 where
  bImap = DV.imap
  {-# INLINABLE bImap #-}

  bMap = DV.map
  {-# INLINABLE bMap #-}

  bUnfoldr = DV.unfoldr
  {-# INLINABLE bUnfoldr #-}

  bUnfoldrN = DV.unfoldrN
  {-# INLINABLE bUnfoldrN #-}

instance VectorLike DV.Vector Word16 where
  toList = DV.toList
  {-# INLINABLE toList #-}

  fromList = DV.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DV.! i
  {-# INLINABLE (!!!) #-}

  concat = DV.concat
  {-# INLINABLE concat #-}

  empty = DV.empty
  {-# INLINABLE empty #-}

  filter = DV.filter
  {-# INLINABLE filter #-}

  generate = DV.generate
  {-# INLINABLE generate #-}

  length = DV.length
  {-# INLINABLE length #-}

  snoc = DV.snoc
  {-# INLINABLE snoc #-}

  sum = DV.sum
  {-# INLINABLE sum #-}

  unsafeIndex = DV.unsafeIndex
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice = DV.unsafeSlice
  {-# INLINABLE unsafeSlice #-}

instance BoxedVectorLike DV.Vector Word16 where
  bImap = DV.imap
  {-# INLINABLE bImap #-}

  bMap = DV.map
  {-# INLINABLE bMap #-}

  bUnfoldr = DV.unfoldr
  {-# INLINABLE bUnfoldr #-}

  bUnfoldrN = DV.unfoldrN
  {-# INLINABLE bUnfoldrN #-}

instance VectorLike DV.Vector Word32 where
  toList = DV.toList
  {-# INLINABLE toList #-}

  fromList = DV.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DV.! i
  {-# INLINABLE (!!!) #-}

  concat = DV.concat
  {-# INLINABLE concat #-}

  empty = DV.empty
  {-# INLINABLE empty #-}

  filter = DV.filter
  {-# INLINABLE filter #-}

  generate = DV.generate
  {-# INLINABLE generate #-}

  length = DV.length
  {-# INLINABLE length #-}

  snoc = DV.snoc
  {-# INLINABLE snoc #-}

  sum = DV.sum
  {-# INLINABLE sum #-}

  unsafeIndex = DV.unsafeIndex
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice = DV.unsafeSlice
  {-# INLINABLE unsafeSlice #-}

instance BoxedVectorLike DV.Vector Word32 where
  bImap = DV.imap
  {-# INLINABLE bImap #-}

  bMap = DV.map
  {-# INLINABLE bMap #-}

  bUnfoldr = DV.unfoldr
  {-# INLINABLE bUnfoldr #-}

  bUnfoldrN = DV.unfoldrN
  {-# INLINABLE bUnfoldrN #-}

instance VectorLike DV.Vector Word64 where
  toList = DV.toList
  {-# INLINABLE toList #-}

  fromList = DV.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DV.! i
  {-# INLINABLE (!!!) #-}

  concat = DV.concat
  {-# INLINABLE concat #-}

  empty = DV.empty
  {-# INLINABLE empty #-}

  filter = DV.filter
  {-# INLINABLE filter #-}

  generate = DV.generate
  {-# INLINABLE generate #-}

  length = DV.length
  {-# INLINABLE length #-}

  snoc = DV.snoc
  {-# INLINABLE snoc #-}

  sum = DV.sum
  {-# INLINABLE sum #-}

  unsafeIndex = DV.unsafeIndex
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice = DV.unsafeSlice
  {-# INLINABLE unsafeSlice #-}

instance BoxedVectorLike DV.Vector Word64 where
  bImap = DV.imap
  {-# INLINABLE bImap #-}

  bMap = DV.map
  {-# INLINABLE bMap #-}

  bUnfoldr = DV.unfoldr
  {-# INLINABLE bUnfoldr #-}

  bUnfoldrN = DV.unfoldrN
  {-# INLINABLE bUnfoldrN #-}

instance VectorLike DVS.Vector Word8 where
  toList = DVS.toList
  {-# INLINABLE toList #-}

  fromList = DVS.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DVS.! i
  {-# INLINABLE (!!!) #-}

  concat = DVS.concat
  {-# INLINABLE concat #-}

  empty = DVS.empty
  {-# INLINABLE empty #-}

  filter = DVS.filter
  {-# INLINABLE filter #-}

  generate = DVS.generate
  {-# INLINABLE generate #-}

  length = DVS.length
  {-# INLINABLE length #-}

  snoc = DVS.snoc
  {-# INLINABLE snoc #-}

  sum = DVS.sum
  {-# INLINABLE sum #-}

  unsafeIndex = DVS.unsafeIndex
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice = DVS.unsafeSlice
  {-# INLINABLE unsafeSlice #-}

instance StorableVectorLike DVS.Vector Word8 where
  sImap = DVS.imap
  {-# INLINABLE sImap #-}

  sMap = DVS.map
  {-# INLINABLE sMap #-}

  sUnfoldr = DVS.unfoldr
  {-# INLINABLE sUnfoldr #-}

  sUnfoldrN = DVS.unfoldrN
  {-# INLINABLE sUnfoldrN #-}

instance VectorLike DVS.Vector Word16 where
  toList = DVS.toList
  {-# INLINABLE toList #-}

  fromList = DVS.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DVS.! i
  {-# INLINABLE (!!!) #-}

  concat = DVS.concat
  {-# INLINABLE concat #-}

  empty = DVS.empty
  {-# INLINABLE empty #-}

  filter = DVS.filter
  {-# INLINABLE filter #-}

  generate = DVS.generate
  {-# INLINABLE generate #-}

  length = DVS.length
  {-# INLINABLE length #-}

  snoc = DVS.snoc
  {-# INLINABLE snoc #-}

  sum = DVS.sum
  {-# INLINABLE sum #-}

  unsafeIndex = DVS.unsafeIndex
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice = DVS.unsafeSlice
  {-# INLINABLE unsafeSlice #-}

instance StorableVectorLike DVS.Vector Word16 where
  sImap = DVS.imap
  {-# INLINABLE sImap #-}

  sMap = DVS.map
  {-# INLINABLE sMap #-}

  sUnfoldr = DVS.unfoldr
  {-# INLINABLE sUnfoldr #-}

  sUnfoldrN = DVS.unfoldrN
  {-# INLINABLE sUnfoldrN #-}

instance VectorLike DVS.Vector Word32 where
  toList = DVS.toList
  {-# INLINABLE toList #-}

  fromList = DVS.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DVS.! i
  {-# INLINABLE (!!!) #-}

  concat = DVS.concat
  {-# INLINABLE concat #-}

  empty = DVS.empty
  {-# INLINABLE empty #-}

  filter = DVS.filter
  {-# INLINABLE filter #-}

  generate = DVS.generate
  {-# INLINABLE generate #-}

  length = DVS.length
  {-# INLINABLE length #-}

  snoc = DVS.snoc
  {-# INLINABLE snoc #-}

  sum = DVS.sum
  {-# INLINABLE sum #-}

  unsafeIndex = DVS.unsafeIndex
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice = DVS.unsafeSlice
  {-# INLINABLE unsafeSlice #-}

instance StorableVectorLike DVS.Vector Word32 where
  sImap = DVS.imap
  {-# INLINABLE sImap #-}

  sMap = DVS.map
  {-# INLINABLE sMap #-}

  sUnfoldr = DVS.unfoldr
  {-# INLINABLE sUnfoldr #-}

  sUnfoldrN = DVS.unfoldrN
  {-# INLINABLE sUnfoldrN #-}

instance VectorLike DVS.Vector Word64 where
  toList = DVS.toList
  {-# INLINABLE toList #-}

  fromList = DVS.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DVS.! i
  {-# INLINABLE (!!!) #-}

  concat = DVS.concat
  {-# INLINABLE concat #-}

  empty = DVS.empty
  {-# INLINABLE empty #-}

  filter = DVS.filter
  {-# INLINABLE filter #-}

  generate = DVS.generate
  {-# INLINABLE generate #-}

  length = DVS.length
  {-# INLINABLE length #-}

  snoc = DVS.snoc
  {-# INLINABLE snoc #-}

  sum = DVS.sum
  {-# INLINABLE sum #-}

  unsafeIndex = DVS.unsafeIndex
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice = DVS.unsafeSlice
  {-# INLINABLE unsafeSlice #-}

instance StorableVectorLike DVS.Vector Word64 where
  sImap = DVS.imap
  {-# INLINABLE sImap #-}

  sMap = DVS.map
  {-# INLINABLE sMap #-}

  sUnfoldr = DVS.unfoldr
  {-# INLINABLE sUnfoldr #-}

  sUnfoldrN = DVS.unfoldrN
  {-# INLINABLE sUnfoldrN #-}
