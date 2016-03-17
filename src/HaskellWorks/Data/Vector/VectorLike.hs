{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Vector.VectorLike
  ( VectorLike(..)
  ) where

import qualified Data.ByteString               as BS
import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.Positioning

class VectorLike v where
  type Elem v

  toList :: v -> [Elem v]
  fromList :: [Elem v] -> v
  (!!!) :: v -> Position -> Elem v
  concat :: [v] -> v
  empty :: v
  filter :: (Elem v -> Bool) -> v -> v
  generate :: Int -> (Int -> Elem v) -> v
  length :: v -> Count
  snoc :: v -> Elem v -> v
  unsafeIndex :: v -> Position -> Elem v
  unsafeSlice :: Position -> Position -> v -> v

instance VectorLike BS.ByteString where
  type Elem BS.ByteString = Word8

  toList = BS.unpack
  {-# INLINABLE toList #-}

  fromList = BS.pack
  {-# INLINABLE fromList #-}

  (!!!) v (Position i) = v `BS.index` fromIntegral i
  {-# INLINABLE (!!!) #-}

  concat = BS.concat
  {-# INLINABLE concat #-}

  empty = BS.empty
  {-# INLINABLE empty #-}

  filter = BS.filter
  {-# INLINABLE filter #-}

  generate n f = fst (BS.unfoldrN n go 0)
    where go i = if i /= n then Just (f i, i + 1) else Nothing
  {-# INLINABLE generate #-}

  length = Count . fromIntegral . BS.length
  {-# INLINABLE length #-}

  snoc = BS.snoc
  {-# INLINABLE snoc #-}

  unsafeIndex v (Position i) = BS.index v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = BS.take (fromIntegral j) . BS.drop (fromIntegral i)
  {-# INLINABLE unsafeSlice #-}

instance VectorLike (DV.Vector Word8) where
  type Elem (DV.Vector Word8) = Word8

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

  unsafeIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}


instance VectorLike (DV.Vector Word16) where
  type Elem (DV.Vector Word16) = Word16

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

  unsafeIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}

instance VectorLike (DV.Vector Word32) where
  type Elem (DV.Vector Word32) = Word32

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

  unsafeIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}

instance VectorLike (DV.Vector Word64) where
  type Elem (DV.Vector Word64) = Word64

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

  unsafeIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}

instance VectorLike (DVS.Vector Word8) where
  type Elem (DVS.Vector Word8) = Word8

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

  unsafeIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}

instance VectorLike (DVS.Vector Word16) where
  type Elem (DVS.Vector Word16) = Word16

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

  unsafeIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}

instance VectorLike (DVS.Vector Word32) where
  type Elem (DVS.Vector Word32) = Word32

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

  unsafeIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}

instance VectorLike (DVS.Vector Word64) where
  type Elem (DVS.Vector Word64) = Word64

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

  unsafeIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINABLE unsafeIndex #-}

  unsafeSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE unsafeSlice #-}
