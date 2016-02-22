{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.VectorLike where

import qualified Data.Vector as DV
import qualified Data.Vector.Storable as DVS
import Data.Word

class VectorLike v e where
  toList :: v e -> [e]
  fromList :: [e] -> v e
  (!!!) :: v e -> Int -> e

instance VectorLike DV.Vector Word8 where
  toList = DV.toList
  {-# INLINABLE toList #-}

  fromList = DV.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DV.! i
  {-# INLINABLE (!!!) #-}

instance VectorLike DV.Vector Word16 where
  toList = DV.toList
  {-# INLINABLE toList #-}

  fromList = DV.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DV.! i
  {-# INLINABLE (!!!) #-}

instance VectorLike DV.Vector Word32 where
  toList = DV.toList
  {-# INLINABLE toList #-}

  fromList = DV.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DV.! i
  {-# INLINABLE (!!!) #-}

instance VectorLike DV.Vector Word64 where
  toList = DV.toList
  {-# INLINABLE toList #-}

  fromList = DV.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DV.! i
  {-# INLINABLE (!!!) #-}

instance VectorLike DVS.Vector Word8 where
  toList = DVS.toList
  {-# INLINABLE toList #-}

  fromList = DVS.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DVS.! i
  {-# INLINABLE (!!!) #-}

instance VectorLike DVS.Vector Word16 where
  toList = DVS.toList
  {-# INLINABLE toList #-}

  fromList = DVS.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DVS.! i
  {-# INLINABLE (!!!) #-}

instance VectorLike DVS.Vector Word32 where
  toList = DVS.toList
  {-# INLINABLE toList #-}

  fromList = DVS.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DVS.! i
  {-# INLINABLE (!!!) #-}

instance VectorLike DVS.Vector Word64 where
  toList = DVS.toList
  {-# INLINABLE toList #-}

  fromList = DVS.fromList
  {-# INLINABLE fromList #-}

  (!!!) v i = v DVS.! i
  {-# INLINABLE (!!!) #-}
