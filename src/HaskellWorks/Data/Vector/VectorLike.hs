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

-- | Class of values that support vector like operations
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
  vFromList = id
  (!!!) v (Position i) = v !! fromIntegral i
  vConcat = concat
  vEmpty = ""
  vFilter = filter
  vGenerate n f = f `fmap` [0 .. (n - 1)]
  vLength = Count . fromIntegral . length
  vSnoc v c = v ++ [c]
  vDrop = drop . fromIntegral
  vTake = take . fromIntegral
  vIndex v (Position i) = v !! fromIntegral i
  vSlice (Position i) (Position j) = take (fromIntegral j) . drop (fromIntegral i)
  {-# INLINE vToList   #-}
  {-# INLINE vFromList #-}
  {-# INLINE (!!!)     #-}
  {-# INLINE vConcat   #-}
  {-# INLINE vEmpty    #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}
  {-# INLINE vLength   #-}
  {-# INLINE vSnoc     #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vIndex    #-}
  {-# INLINE vSlice    #-}

instance VectorLike BS.ByteString where
  type Elem BS.ByteString = Word8

  vToList = BS.unpack
  vFromList = BS.pack
  (!!!) v (Position i) = v `BS.index` fromIntegral i
  vConcat = BS.concat
  vEmpty = BS.empty
  vFilter = BS.filter
  vGenerate n f = fst (BS.unfoldrN n go 0)
    where go i = if i /= n then Just (f i, i + 1) else Nothing
  vLength = Count . fromIntegral . BS.length
  vSnoc = BS.snoc
  vDrop = BS.drop . fromIntegral
  vTake = BS.take . fromIntegral
  vIndex v (Position i) = BS.index v (fromIntegral i)
  vSlice (Position i) (Position j) = BS.take (fromIntegral j) . BS.drop (fromIntegral i)
  {-# INLINE vToList   #-}
  {-# INLINE vFromList #-}
  {-# INLINE (!!!)     #-}
  {-# INLINE vConcat   #-}
  {-# INLINE vEmpty    #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}
  {-# INLINE vLength   #-}
  {-# INLINE vSnoc     #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vIndex    #-}
  {-# INLINE vSlice    #-}

instance VectorLike (DV.Vector Word8) where
  type Elem (DV.Vector Word8) = Word8

  vToList = DV.toList
  vFromList = DV.fromList
  (!!!) v (Position i) = v DV.! fromIntegral i
  vConcat = DV.concat
  vEmpty = DV.empty
  vFilter = DV.filter
  vGenerate = DV.generate
  vLength = Count . fromIntegral . DV.length
  vSnoc = DV.snoc
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  vSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINE vToList   #-}
  {-# INLINE vFromList #-}
  {-# INLINE (!!!)     #-}
  {-# INLINE vConcat   #-}
  {-# INLINE vEmpty    #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}
  {-# INLINE vLength   #-}
  {-# INLINE vSnoc     #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vIndex    #-}
  {-# INLINE vSlice    #-}

instance VectorLike (DV.Vector Word16) where
  type Elem (DV.Vector Word16) = Word16

  vToList = DV.toList
  vFromList = DV.fromList
  (!!!) v (Position i) = v DV.! fromIntegral i
  vConcat = DV.concat
  vEmpty = DV.empty
  vFilter = DV.filter
  vGenerate = DV.generate
  vLength = Count . fromIntegral . DV.length
  vSnoc = DV.snoc
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  vSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINE vToList   #-}
  {-# INLINE vFromList #-}
  {-# INLINE (!!!)     #-}
  {-# INLINE vConcat   #-}
  {-# INLINE vEmpty    #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}
  {-# INLINE vLength   #-}
  {-# INLINE vSnoc     #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vIndex    #-}
  {-# INLINE vSlice    #-}

instance VectorLike (DV.Vector Word32) where
  type Elem (DV.Vector Word32) = Word32

  vToList = DV.toList
  vFromList = DV.fromList
  (!!!) v (Position i) = v DV.! fromIntegral i
  vConcat = DV.concat
  vEmpty = DV.empty
  vFilter = DV.filter
  vGenerate = DV.generate
  vLength = Count . fromIntegral . DV.length
  vSnoc = DV.snoc
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  vSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINE vToList   #-}
  {-# INLINE vFromList #-}
  {-# INLINE (!!!)     #-}
  {-# INLINE vConcat   #-}
  {-# INLINE vEmpty    #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}
  {-# INLINE vLength   #-}
  {-# INLINE vSnoc     #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vIndex    #-}
  {-# INLINE vSlice    #-}

instance VectorLike (DV.Vector Word64) where
  type Elem (DV.Vector Word64) = Word64

  vToList = DV.toList
  vFromList = DV.fromList
  (!!!) v (Position i) = v DV.! fromIntegral i
  vConcat = DV.concat
  vEmpty = DV.empty
  vFilter = DV.filter
  vGenerate = DV.generate
  vLength = Count . fromIntegral . DV.length
  vSnoc = DV.snoc
  vDrop = DV.drop . fromIntegral
  vTake = DV.take . fromIntegral
  vIndex v (Position i) = DV.unsafeIndex v (fromIntegral i)
  vSlice (Position i) (Position j) = DV.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINE vToList   #-}
  {-# INLINE vFromList #-}
  {-# INLINE (!!!)     #-}
  {-# INLINE vConcat   #-}
  {-# INLINE vEmpty    #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}
  {-# INLINE vLength   #-}
  {-# INLINE vSnoc     #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vIndex    #-}
  {-# INLINE vSlice    #-}

instance VectorLike (DVS.Vector Word8) where
  type Elem (DVS.Vector Word8) = Word8

  vToList = DVS.toList
  vFromList = DVS.fromList
  (!!!) v (Position i) = v DVS.! fromIntegral i
  vConcat = DVS.concat
  vEmpty = DVS.empty
  vFilter = DVS.filter
  vGenerate = DVS.generate
  vLength = Count . fromIntegral . DVS.length
  vSnoc = DVS.snoc
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  vSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINE vToList   #-}
  {-# INLINE vFromList #-}
  {-# INLINE (!!!)     #-}
  {-# INLINE vConcat   #-}
  {-# INLINE vEmpty    #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}
  {-# INLINE vLength   #-}
  {-# INLINE vSnoc     #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vIndex    #-}
  {-# INLINE vSlice    #-}

instance VectorLike (DVS.Vector Word16) where
  type Elem (DVS.Vector Word16) = Word16

  vToList = DVS.toList
  vFromList = DVS.fromList
  (!!!) v (Position i) = v DVS.! fromIntegral i
  vConcat = DVS.concat
  vEmpty = DVS.empty
  vFilter = DVS.filter
  vGenerate = DVS.generate
  vLength = Count . fromIntegral . DVS.length
  vSnoc = DVS.snoc
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  vSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINE vToList   #-}
  {-# INLINE vFromList #-}
  {-# INLINE (!!!)     #-}
  {-# INLINE vConcat   #-}
  {-# INLINE vEmpty    #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}
  {-# INLINE vLength   #-}
  {-# INLINE vSnoc     #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vIndex    #-}
  {-# INLINE vSlice    #-}

instance VectorLike (DVS.Vector Word32) where
  type Elem (DVS.Vector Word32) = Word32

  vToList = DVS.toList
  vFromList = DVS.fromList
  (!!!) v (Position i) = v DVS.! fromIntegral i
  vConcat = DVS.concat
  vEmpty = DVS.empty
  vFilter = DVS.filter
  vGenerate = DVS.generate
  vLength = Count . fromIntegral . DVS.length
  vSnoc = DVS.snoc
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  vSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINE vToList   #-}
  {-# INLINE vFromList #-}
  {-# INLINE (!!!)     #-}
  {-# INLINE vConcat   #-}
  {-# INLINE vEmpty    #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}
  {-# INLINE vLength   #-}
  {-# INLINE vSnoc     #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vIndex    #-}
  {-# INLINE vSlice    #-}

instance VectorLike (DVS.Vector Word64) where
  type Elem (DVS.Vector Word64) = Word64

  vToList = DVS.toList
  vFromList = DVS.fromList
  (!!!) v (Position i) = v DVS.! fromIntegral i
  vConcat = DVS.concat
  vEmpty = DVS.empty
  vFilter = DVS.filter
  vGenerate = DVS.generate
  vLength = Count . fromIntegral . DVS.length
  vSnoc = DVS.snoc
  vDrop = DVS.drop . fromIntegral
  vTake = DVS.take . fromIntegral
  vIndex v (Position i) = DVS.unsafeIndex v (fromIntegral i)
  vSlice (Position i) (Position j) = DVS.unsafeSlice (fromIntegral i) (fromIntegral j)
  {-# INLINE vToList   #-}
  {-# INLINE vFromList #-}
  {-# INLINE (!!!)     #-}
  {-# INLINE vConcat   #-}
  {-# INLINE vEmpty    #-}
  {-# INLINE vFilter   #-}
  {-# INLINE vGenerate #-}
  {-# INLINE vLength   #-}
  {-# INLINE vSnoc     #-}
  {-# INLINE vDrop     #-}
  {-# INLINE vTake     #-}
  {-# INLINE vIndex    #-}
  {-# INLINE vSlice    #-}
