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

  vToList :: v -> [Elem v]
  vFromList :: [Elem v] -> v
  (!!!) :: v -> Position -> Elem v
  vConcat :: [v] -> v
  vEmpty :: v
  vFilter :: (Elem v -> Bool) -> v -> v
  vGenerate :: Int -> (Int -> Elem v) -> v
  vLength :: v -> Count
  vSnoc :: v -> Elem v -> v
  vDrop :: Count -> v -> v
  vTake :: Count -> v -> v
  vIndex :: v -> Position -> Elem v
  vSlice :: Position -> Position -> v -> v


instance VectorLike String where
  type Elem String = Char

  vToList = id
  {-# INLINABLE vToList #-}

  vFromList = id
  {-# INLINABLE vFromList #-}

  (!!!) v (Position i) = v !! fromIntegral i
  {-# INLINABLE (!!!) #-}

  vConcat = concat
  {-# INLINABLE vConcat #-}

  vEmpty = ""
  {-# INLINABLE vEmpty #-}

  vFilter = filter
  {-# INLINABLE vFilter #-}

  vGenerate n f = f `fmap` [0 .. (n - 1)]
  {-# INLINABLE vGenerate #-}

  vLength = Count . fromIntegral . length
  {-# INLINABLE vLength #-}

  vSnoc v c = v ++ [c]
  {-# INLINABLE vSnoc #-}

  vDrop = drop . fromIntegral
  {-# INLINABLE vDrop #-}

  vTake = take . fromIntegral
  {-# INLINABLE vTake #-}

  vIndex v (Position i) = v !! fromIntegral i
  {-# INLINABLE vIndex #-}

  vSlice (Position i) (Position j) = take (fromIntegral j) . drop (fromIntegral i)
  {-# INLINABLE vSlice #-}

instance VectorLike BS.ByteString where
  type Elem BS.ByteString = Word8

  vToList = BS.unpack
  {-# INLINABLE vToList #-}

  vFromList = BS.pack
  {-# INLINABLE vFromList #-}

  (!!!) v (Position i) = v `BS.index` fromIntegral i
  {-# INLINABLE (!!!) #-}

  vConcat = BS.concat
  {-# INLINABLE vConcat #-}

  vEmpty = BS.empty
  {-# INLINABLE vEmpty #-}

  vFilter = BS.filter
  {-# INLINABLE vFilter #-}

  vGenerate n f = fst (BS.unfoldrN n go 0)
    where go i = if i /= n then Just (f i, i + 1) else Nothing
  {-# INLINABLE vGenerate #-}

  vLength = Count . fromIntegral . BS.length
  {-# INLINABLE vLength #-}

  vSnoc = BS.snoc
  {-# INLINABLE vSnoc #-}

  vDrop = BS.drop . fromIntegral
  {-# INLINABLE vDrop #-}

  vTake = BS.take . fromIntegral
  {-# INLINABLE vTake #-}

  vIndex v (Position i) = BS.index v (fromIntegral i)
  {-# INLINABLE vIndex #-}

  vSlice (Position i) (Position j) = BS.take (fromIntegral j) . BS.drop (fromIntegral i)
  {-# INLINABLE vSlice #-}

instance VectorLike (DV.Vector Word8) where
  type Elem (DV.Vector Word8) = Word8

  vToList = DV.toList
  {-# INLINABLE vToList #-}

  vFromList = DV.fromList
  {-# INLINABLE vFromList #-}

  (!!!) v (Position i) = v DV.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  vConcat = DV.concat
  {-# INLINABLE vConcat #-}

  vEmpty = DV.empty
  {-# INLINABLE vEmpty #-}

  vFilter = DV.filter
  {-# INLINABLE vFilter #-}

  vGenerate = DV.generate
  {-# INLINABLE vGenerate #-}

  vLength = Count . fromIntegral . DV.length
  {-# INLINABLE vLength #-}

  vSnoc = DV.snoc
  {-# INLINABLE vSnoc #-}

  vDrop = DV.drop . fromIntegral
  {-# INLINABLE vDrop #-}

  vTake = DV.take . fromIntegral
  {-# INLINABLE vTake #-}

  vIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINABLE vIndex #-}

  vSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE vSlice #-}


instance VectorLike (DV.Vector Word16) where
  type Elem (DV.Vector Word16) = Word16

  vToList = DV.toList
  {-# INLINABLE vToList #-}

  vFromList = DV.fromList
  {-# INLINABLE vFromList #-}

  (!!!) v (Position i) = v DV.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  vConcat = DV.concat
  {-# INLINABLE vConcat #-}

  vEmpty = DV.empty
  {-# INLINABLE vEmpty #-}

  vFilter = DV.filter
  {-# INLINABLE vFilter #-}

  vGenerate = DV.generate
  {-# INLINABLE vGenerate #-}

  vLength = Count . fromIntegral . DV.length
  {-# INLINABLE vLength #-}

  vSnoc = DV.snoc
  {-# INLINABLE vSnoc #-}

  vDrop = DV.drop . fromIntegral
  {-# INLINABLE vDrop #-}

  vTake = DV.take . fromIntegral
  {-# INLINABLE vTake #-}

  vIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINABLE vIndex #-}

  vSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE vSlice #-}

instance VectorLike (DV.Vector Word32) where
  type Elem (DV.Vector Word32) = Word32

  vToList = DV.toList
  {-# INLINABLE vToList #-}

  vFromList = DV.fromList
  {-# INLINABLE vFromList #-}

  (!!!) v (Position i) = v DV.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  vConcat = DV.concat
  {-# INLINABLE vConcat #-}

  vEmpty = DV.empty
  {-# INLINABLE vEmpty #-}

  vFilter = DV.filter
  {-# INLINABLE vFilter #-}

  vGenerate = DV.generate
  {-# INLINABLE vGenerate #-}

  vLength = Count . fromIntegral . DV.length
  {-# INLINABLE vLength #-}

  vSnoc = DV.snoc
  {-# INLINABLE vSnoc #-}

  vDrop = DV.drop . fromIntegral
  {-# INLINABLE vDrop #-}

  vTake = DV.take . fromIntegral
  {-# INLINABLE vTake #-}

  vIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINABLE vIndex #-}

  vSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE vSlice #-}

instance VectorLike (DV.Vector Word64) where
  type Elem (DV.Vector Word64) = Word64

  vToList = DV.toList
  {-# INLINABLE vToList #-}

  vFromList = DV.fromList
  {-# INLINABLE vFromList #-}

  (!!!) v (Position i) = v DV.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  vConcat = DV.concat
  {-# INLINABLE vConcat #-}

  vEmpty = DV.empty
  {-# INLINABLE vEmpty #-}

  vFilter = DV.filter
  {-# INLINABLE vFilter #-}

  vGenerate = DV.generate
  {-# INLINABLE vGenerate #-}

  vLength = Count . fromIntegral . DV.length
  {-# INLINABLE vLength #-}

  vSnoc = DV.snoc
  {-# INLINABLE vSnoc #-}

  vDrop = DV.drop . fromIntegral
  {-# INLINABLE vDrop #-}

  vTake = DV.take . fromIntegral
  {-# INLINABLE vTake #-}

  vIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  {-# INLINABLE vIndex #-}

  vSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE vSlice #-}

instance VectorLike (DVS.Vector Word8) where
  type Elem (DVS.Vector Word8) = Word8

  vToList = DVS.toList
  {-# INLINABLE vToList #-}

  vFromList = DVS.fromList
  {-# INLINABLE vFromList #-}

  (!!!) v (Position i) = v DVS.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  vConcat = DVS.concat
  {-# INLINABLE vConcat #-}

  vEmpty = DVS.empty
  {-# INLINABLE vEmpty #-}

  vFilter = DVS.filter
  {-# INLINABLE vFilter #-}

  vGenerate = DVS.generate
  {-# INLINABLE vGenerate #-}

  vLength = Count . fromIntegral . DVS.length
  {-# INLINABLE vLength #-}

  vSnoc = DVS.snoc
  {-# INLINABLE vSnoc #-}

  vDrop = DVS.drop . fromIntegral
  {-# INLINABLE vDrop #-}

  vTake = DVS.take . fromIntegral
  {-# INLINABLE vTake #-}

  vIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINABLE vIndex #-}

  vSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE vSlice #-}

instance VectorLike (DVS.Vector Word16) where
  type Elem (DVS.Vector Word16) = Word16

  vToList = DVS.toList
  {-# INLINABLE vToList #-}

  vFromList = DVS.fromList
  {-# INLINABLE vFromList #-}

  (!!!) v (Position i) = v DVS.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  vConcat = DVS.concat
  {-# INLINABLE vConcat #-}

  vEmpty = DVS.empty
  {-# INLINABLE vEmpty #-}

  vFilter = DVS.filter
  {-# INLINABLE vFilter #-}

  vGenerate = DVS.generate
  {-# INLINABLE vGenerate #-}

  vLength = Count . fromIntegral . DVS.length
  {-# INLINABLE vLength #-}

  vSnoc = DVS.snoc
  {-# INLINABLE vSnoc #-}

  vDrop = DVS.drop . fromIntegral
  {-# INLINABLE vDrop #-}

  vTake = DVS.take . fromIntegral
  {-# INLINABLE vTake #-}

  vIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINABLE vIndex #-}

  vSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE vSlice #-}

instance VectorLike (DVS.Vector Word32) where
  type Elem (DVS.Vector Word32) = Word32

  vToList = DVS.toList
  {-# INLINABLE vToList #-}

  vFromList = DVS.fromList
  {-# INLINABLE vFromList #-}

  (!!!) v (Position i) = v DVS.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  vConcat = DVS.concat
  {-# INLINABLE vConcat #-}

  vEmpty = DVS.empty
  {-# INLINABLE vEmpty #-}

  vFilter = DVS.filter
  {-# INLINABLE vFilter #-}

  vGenerate = DVS.generate
  {-# INLINABLE vGenerate #-}

  vLength = Count . fromIntegral . DVS.length
  {-# INLINABLE vLength #-}

  vSnoc = DVS.snoc
  {-# INLINABLE vSnoc #-}

  vDrop = DVS.drop . fromIntegral
  {-# INLINABLE vDrop #-}

  vTake = DVS.take . fromIntegral
  {-# INLINABLE vTake #-}

  vIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINABLE vIndex #-}

  vSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE vSlice #-}

instance VectorLike (DVS.Vector Word64) where
  type Elem (DVS.Vector Word64) = Word64

  vToList = DVS.toList
  {-# INLINABLE vToList #-}

  vFromList = DVS.fromList
  {-# INLINABLE vFromList #-}

  (!!!) v (Position i) = v DVS.! fromIntegral i
  {-# INLINABLE (!!!) #-}

  vConcat = DVS.concat
  {-# INLINABLE vConcat #-}

  vEmpty = DVS.empty
  {-# INLINABLE vEmpty #-}

  vFilter = DVS.filter
  {-# INLINABLE vFilter #-}

  vGenerate = DVS.generate
  {-# INLINABLE vGenerate #-}

  vLength = Count . fromIntegral . DVS.length
  {-# INLINABLE vLength #-}

  vSnoc = DVS.snoc
  {-# INLINABLE vSnoc #-}

  vDrop = DVS.drop . fromIntegral
  {-# INLINABLE vDrop #-}

  vTake = DVS.take . fromIntegral
  {-# INLINABLE vTake #-}

  vIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  {-# INLINABLE vIndex #-}

  vSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINABLE vSlice #-}
