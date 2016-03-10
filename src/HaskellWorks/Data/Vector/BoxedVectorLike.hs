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
  {-# INLINABLE bImap #-}

  bMap = DV.map
  {-# INLINABLE bMap #-}

  bUnfoldr = DV.unfoldr
  {-# INLINABLE bUnfoldr #-}

  bUnfoldrN = DV.unfoldrN
  {-# INLINABLE bUnfoldrN #-}

instance BoxedVectorLike DV.Vector Word16 where
  bImap = DV.imap
  {-# INLINABLE bImap #-}

  bMap = DV.map
  {-# INLINABLE bMap #-}

  bUnfoldr = DV.unfoldr
  {-# INLINABLE bUnfoldr #-}

  bUnfoldrN = DV.unfoldrN
  {-# INLINABLE bUnfoldrN #-}

instance BoxedVectorLike DV.Vector Word32 where
  bImap = DV.imap
  {-# INLINABLE bImap #-}

  bMap = DV.map
  {-# INLINABLE bMap #-}

  bUnfoldr = DV.unfoldr
  {-# INLINABLE bUnfoldr #-}

  bUnfoldrN = DV.unfoldrN
  {-# INLINABLE bUnfoldrN #-}

instance BoxedVectorLike DV.Vector Word64 where
  bImap = DV.imap
  {-# INLINABLE bImap #-}

  bMap = DV.map
  {-# INLINABLE bMap #-}

  bUnfoldr = DV.unfoldr
  {-# INLINABLE bUnfoldr #-}

  bUnfoldrN = DV.unfoldrN
  {-# INLINABLE bUnfoldrN #-}
