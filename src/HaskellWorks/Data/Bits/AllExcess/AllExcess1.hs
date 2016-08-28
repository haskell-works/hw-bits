{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Bits.AllExcess.AllExcess1 where

import           Data.Word
import qualified Data.Vector                                as DV
import qualified Data.Vector.Storable                       as DVS
import           HaskellWorks.Data.Bits.PopCount.PopCount0
import           HaskellWorks.Data.Bits.PopCount.PopCount1

-- TODO Optimise these instances
class AllExcess1 a where
  allExcess1 :: a -> Int

instance AllExcess1 [Bool] where
  allExcess1 = go 0
    where go n (True :ys) = go (n + 1) ys
          go n (False:ys) = go (n - 1) ys
          go n _          = n
  {-# INLINE allExcess1 #-}

instance AllExcess1 Word8 where
  allExcess1 w = fromIntegral (popCount1 w) - fromIntegral (popCount0 w)
  {-# INLINE allExcess1 #-}

instance AllExcess1 Word16 where
  allExcess1 w = fromIntegral (popCount1 w) - fromIntegral (popCount0 w)
  {-# INLINE allExcess1 #-}

instance AllExcess1 Word32 where
  allExcess1 w = fromIntegral (popCount1 w) - fromIntegral (popCount0 w)
  {-# INLINE allExcess1 #-}

instance AllExcess1 Word64 where
  allExcess1 w = fromIntegral (popCount1 w) - fromIntegral (popCount0 w)
  {-# INLINE allExcess1 #-}

instance AllExcess1 (DV.Vector Word8) where
  allExcess1 w = fromIntegral (popCount1 w) - fromIntegral (popCount0 w)
  {-# INLINE allExcess1 #-}

instance AllExcess1 (DV.Vector Word16) where
  allExcess1 w = fromIntegral (popCount1 w) - fromIntegral (popCount0 w)
  {-# INLINE allExcess1 #-}

instance AllExcess1 (DV.Vector Word32) where
  allExcess1 w = fromIntegral (popCount1 w) - fromIntegral (popCount0 w)
  {-# INLINE allExcess1 #-}

instance AllExcess1 (DV.Vector Word64) where
  allExcess1 w = fromIntegral (popCount1 w) - fromIntegral (popCount0 w)
  {-# INLINE allExcess1 #-}

instance AllExcess1 (DVS.Vector Word8) where
  allExcess1 w = fromIntegral (popCount1 w) - fromIntegral (popCount0 w)
  {-# INLINE allExcess1 #-}

instance AllExcess1 (DVS.Vector Word16) where
  allExcess1 w = fromIntegral (popCount1 w) - fromIntegral (popCount0 w)
  {-# INLINE allExcess1 #-}

instance AllExcess1 (DVS.Vector Word32) where
  allExcess1 w = fromIntegral (popCount1 w) - fromIntegral (popCount0 w)
  {-# INLINE allExcess1 #-}

instance AllExcess1 (DVS.Vector Word64) where
  allExcess1 w = fromIntegral (popCount1 w) - fromIntegral (popCount0 w)
  {-# INLINE allExcess1 #-}
