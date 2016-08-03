{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Bits.AllExcess.AllExcess0 where

import           Data.Word
import qualified Data.Vector.Storable             as DVS
import           HaskellWorks.Data.Bits.PopCount.PopCount0
import           HaskellWorks.Data.Bits.PopCount.PopCount1

-- TODO Optimise these instances
class AllExcess0 a where
  allExcess0 :: a -> Int

instance AllExcess0 Word8 where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 Word16 where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 Word32 where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 Word64 where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 (DVS.Vector Word8) where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 (DVS.Vector Word16) where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 (DVS.Vector Word32) where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}

instance AllExcess0 (DVS.Vector Word64) where
  allExcess0 w = fromIntegral (popCount0 w) - fromIntegral (popCount1 w)
  {-# INLINE allExcess0 #-}
