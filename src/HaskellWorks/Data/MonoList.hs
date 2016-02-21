{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.MonoList where

import qualified Data.Vector as DV
import qualified Data.Vector.Storable as DVS
import Data.Word

class MonoList v where
  type Elem v
  monoList :: v -> [Elem v]

class MonoList v => Indexable v where
  (!!!) :: v -> Int -> Elem v

instance MonoList (DV.Vector Word8) where
  type Elem (DV.Vector Word8) = Word8
  monoList = DV.toList
  {-# INLINABLE monoList #-}

instance MonoList (DV.Vector Word16) where
  type Elem (DV.Vector Word16) = Word16
  monoList = DV.toList
  {-# INLINABLE monoList #-}

instance MonoList (DV.Vector Word32) where
  type Elem (DV.Vector Word32) = Word32
  monoList = DV.toList
  {-# INLINABLE monoList #-}

instance MonoList (DV.Vector Word64) where
  type Elem (DV.Vector Word64) = Word64
  monoList = DV.toList
  {-# INLINABLE monoList #-}

instance MonoList (DVS.Vector Word8) where
  type Elem (DVS.Vector Word8) = Word8
  monoList = DVS.toList
  {-# INLINABLE monoList #-}

instance MonoList (DVS.Vector Word16) where
  type Elem (DVS.Vector Word16) = Word16
  monoList = DVS.toList
  {-# INLINABLE monoList #-}

instance MonoList (DVS.Vector Word32) where
  type Elem (DVS.Vector Word32) = Word32
  monoList = DVS.toList
  {-# INLINABLE monoList #-}

instance MonoList (DVS.Vector Word64) where
  type Elem (DVS.Vector Word64) = Word64
  monoList = DVS.toList
  {-# INLINABLE monoList #-}

instance Indexable (DV.Vector Word8) where
  (!!!) v i = v DV.! i
  {-# INLINABLE (!!!) #-}

instance Indexable (DV.Vector Word16) where
  (!!!) v i = v DV.! i
  {-# INLINABLE (!!!) #-}

instance Indexable (DV.Vector Word32) where
  (!!!) v i = v DV.! i
  {-# INLINABLE (!!!) #-}

instance Indexable (DV.Vector Word64) where
  (!!!) v i = v DV.! i
  {-# INLINABLE (!!!) #-}

instance Indexable (DVS.Vector Word8) where
  (!!!) v i = v DVS.! i
  {-# INLINABLE (!!!) #-}

instance Indexable (DVS.Vector Word16) where
  (!!!) v i = v DVS.! i
  {-# INLINABLE (!!!) #-}

instance Indexable (DVS.Vector Word32) where
  (!!!) v i = v DVS.! i
  {-# INLINABLE (!!!) #-}

instance Indexable (DVS.Vector Word64) where
  (!!!) v i = v DVS.! i
  {-# INLINABLE (!!!) #-}
