{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Vector.BoxedVectorLike
  ( BoxedVectorLike(..)
  ) where

import qualified Data.Vector      as DV
import           Data.Word
import           Foreign.Storable

class BoxedVectorLike v e where
  bImap :: (Int -> a -> b) -> v a -> v b
  bMap :: (a -> b) -> v a -> v b
  bUnfoldr :: (Storable a) => (b -> Maybe (a, b)) -> b -> v a
  bUnfoldrN :: (Storable a) => Int -> (b -> Maybe (a, b)) -> b -> v a

instance BoxedVectorLike DV.Vector Word8 where
  bImap = DV.imap
  bMap = DV.map
  bUnfoldr = DV.unfoldr
  bUnfoldrN = DV.unfoldrN
  {-# INLINABLE bImap     #-}
  {-# INLINABLE bMap      #-}
  {-# INLINABLE bUnfoldr  #-}
  {-# INLINABLE bUnfoldrN #-}

instance BoxedVectorLike DV.Vector Word16 where
  bImap = DV.imap
  bMap = DV.map
  bUnfoldr = DV.unfoldr
  bUnfoldrN = DV.unfoldrN
  {-# INLINABLE bImap     #-}
  {-# INLINABLE bMap      #-}
  {-# INLINABLE bUnfoldr  #-}
  {-# INLINABLE bUnfoldrN #-}

instance BoxedVectorLike DV.Vector Word32 where
  bImap = DV.imap
  bMap = DV.map
  bUnfoldr = DV.unfoldr
  bUnfoldrN = DV.unfoldrN
  {-# INLINABLE bImap     #-}
  {-# INLINABLE bMap      #-}
  {-# INLINABLE bUnfoldr  #-}
  {-# INLINABLE bUnfoldrN #-}

instance BoxedVectorLike DV.Vector Word64 where
  bImap = DV.imap
  bMap = DV.map
  bUnfoldr = DV.unfoldr
  bUnfoldrN = DV.unfoldrN
  {-# INLINABLE bImap     #-}
  {-# INLINABLE bMap      #-}
  {-# INLINABLE bUnfoldr  #-}
  {-# INLINABLE bUnfoldrN #-}
