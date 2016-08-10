{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Bits.Unmatched
  ( UnmatchedL0(..)
  , UnmatchedL1(..)
  , UnmatchedR0(..)
  , UnmatchedR1(..)
  ) where

import qualified Data.Vector.Storable               as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.FixedBitSize
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning

bitEnd :: FixedBitSize w => w -> Position
bitEnd = toPosition . fixedBitSize
{-# INLINE bitEnd #-}

goL0 :: (TestBit w, FixedBitSize w) => Position -> Int -> w -> Int
goL0 n c w = if 0 <= n && n < bitEnd w
  then let delta = if w .?. (bitEnd w - n - 1) then -1 else 1 in goL0 (n + 1) ((c + delta) `max` 0) w
  else c
{-# INLINE goL0 #-}

goL1 :: (TestBit w, FixedBitSize w) => Position -> Int -> w -> Int
goL1 n c w = if 0 <= n && n < bitEnd w
  then let delta = if w .?. (bitEnd w - n - 1) then 1 else -1 in goL1 (n + 1) ((c + delta) `max` 0) w
  else c
{-# INLINE goL1 #-}

goR0 :: (TestBit w, FixedBitSize w) => Position -> Int -> w -> Int
goR0 n c w = if 0 <= n && n < bitEnd w
  then let delta = if w .?. n then -1 else 1 in goR0 (n + 1) ((c + delta) `max` 0) w
  else c
{-# INLINE goR0 #-}

goR1 :: (TestBit w, FixedBitSize w) => Position -> Int -> w -> Int
goR1 n c w = if 0 <= n && n < bitEnd w
  then let delta = if w .?. n then 1 else -1 in goR1 (n + 1) ((c + delta) `max` 0) w
  else c
{-# INLINE goR1 #-}

goDVSL0 :: (UnmatchedL0 w, UnmatchedR1 w, DVS.Storable w) => Int -> DVS.Vector w -> Int
goDVSL0 ub v = if DVS.length v == 0
  then ub
  else let a = DVS.last v in goDVSL0 (unmatchedL0 a + ((ub - unmatchedR1 a) `max` 0)) (DVS.init v)
{-# INLINE goDVSL0 #-}

goDVSL1 :: (UnmatchedL1 w, UnmatchedR0 w, DVS.Storable w) => Int -> DVS.Vector w -> Int
goDVSL1 ub v = if DVS.length v == 0
  then ub
  else let a = DVS.last v in goDVSL1 (unmatchedL1 a + ((ub - unmatchedR0 a) `max` 0)) (DVS.init v)
{-# INLINE goDVSL1 #-}

goDVSR0 :: (UnmatchedR0 w, UnmatchedL1 w, DVS.Storable w) => Int -> DVS.Vector w -> Int
goDVSR0 ua v = if DVS.length v == 0
  then ua
  else let b = DVS.head v in goDVSR0 (unmatchedR0 b + ((ua - unmatchedL1 b) `max` 0)) (DVS.tail v)
{-# INLINE goDVSR0 #-}

goDVSR1 :: (UnmatchedR1 w, UnmatchedL0 w, DVS.Storable w) => Int -> DVS.Vector w -> Int
goDVSR1 ub v = if DVS.length v == 0
  then ub
  else let a = DVS.head v in goDVSR1 (unmatchedR1 a + ((ub - unmatchedL0 a) `max` 0)) (DVS.tail v)
{-# INLINE goDVSR1 #-}

class UnmatchedL0 a where
  unmatchedL0 :: a -> Int

class UnmatchedL1 a where
  unmatchedL1 :: a -> Int

class UnmatchedR0 a where
  unmatchedR0 :: a -> Int

class UnmatchedR1 a where
  unmatchedR1 :: a -> Int

instance UnmatchedL0 Word8 where
  unmatchedL0 = goL0 0 0
  {-# INLINE unmatchedL0 #-}

instance UnmatchedL0 Word16 where
  unmatchedL0 = goL0 0 0
  {-# INLINE unmatchedL0 #-}

instance UnmatchedL0 Word32 where
  unmatchedL0 = goL0 0 0
  {-# INLINE unmatchedL0 #-}

instance UnmatchedL0 Word64 where
  unmatchedL0 = goL0 0 0
  {-# INLINE unmatchedL0 #-}

instance UnmatchedL0 (DVS.Vector Word8) where
  unmatchedL0 = goDVSL0 0
  {-# INLINE unmatchedL0 #-}

instance UnmatchedL0 (DVS.Vector Word16) where
  unmatchedL0 = goDVSL0 0
  {-# INLINE unmatchedL0 #-}

instance UnmatchedL0 (DVS.Vector Word32) where
  unmatchedL0 = goDVSL0 0
  {-# INLINE unmatchedL0 #-}

instance UnmatchedL0 (DVS.Vector Word64) where
  unmatchedL0 = goDVSL0 0
  {-# INLINE unmatchedL0 #-}

instance UnmatchedL1 Word8 where
  unmatchedL1 = goL1 0 0
  {-# INLINE unmatchedL1 #-}

instance UnmatchedL1 Word16 where
  unmatchedL1 = goL1 0 0
  {-# INLINE unmatchedL1 #-}

instance UnmatchedL1 Word32 where
  unmatchedL1 = goL1 0 0
  {-# INLINE unmatchedL1 #-}

instance UnmatchedL1 Word64 where
  unmatchedL1 = goL1 0 0
  {-# INLINE unmatchedL1 #-}

instance UnmatchedL1 (DVS.Vector Word8) where
  unmatchedL1 = goDVSL1 0
  {-# INLINE unmatchedL1 #-}

instance UnmatchedL1 (DVS.Vector Word16) where
  unmatchedL1 = goDVSL1 0
  {-# INLINE unmatchedL1 #-}

instance UnmatchedL1 (DVS.Vector Word32) where
  unmatchedL1 = goDVSL1 0
  {-# INLINE unmatchedL1 #-}

instance UnmatchedL1 (DVS.Vector Word64) where
  unmatchedL1 = goDVSL1 0
  {-# INLINE unmatchedL1 #-}

instance UnmatchedR0 Word8 where
  unmatchedR0 = goR0 0 0
  {-# INLINE unmatchedR0 #-}

instance UnmatchedR0 Word16 where
  unmatchedR0 = goR0 0 0
  {-# INLINE unmatchedR0 #-}

instance UnmatchedR0 Word32 where
  unmatchedR0 = goR0 0 0
  {-# INLINE unmatchedR0 #-}

instance UnmatchedR0 Word64 where
  unmatchedR0 = goR0 0 0
  {-# INLINE unmatchedR0 #-}

instance UnmatchedR0 (DVS.Vector Word8) where
  unmatchedR0 = goDVSR0 0
  {-# INLINE unmatchedR0 #-}

instance UnmatchedR0 (DVS.Vector Word16) where
  unmatchedR0 = goDVSR0 0
  {-# INLINE unmatchedR0 #-}

instance UnmatchedR0 (DVS.Vector Word32) where
  unmatchedR0 = goDVSR0 0
  {-# INLINE unmatchedR0 #-}

instance UnmatchedR0 (DVS.Vector Word64) where
  unmatchedR0 = goDVSR0 0
  {-# INLINE unmatchedR0 #-}

instance UnmatchedR1 Word8 where
  unmatchedR1 = goR1 0 0
  {-# INLINE unmatchedR1 #-}

instance UnmatchedR1 Word16 where
  unmatchedR1 = goR1 0 0
  {-# INLINE unmatchedR1 #-}

instance UnmatchedR1 Word32 where
  unmatchedR1 = goR1 0 0
  {-# INLINE unmatchedR1 #-}

instance UnmatchedR1 Word64 where
  unmatchedR1 = goR1 0 0
  {-# INLINE unmatchedR1 #-}

instance UnmatchedR1 (DVS.Vector Word8) where
  unmatchedR1 = goDVSR1 0
  {-# INLINE unmatchedR1 #-}

instance UnmatchedR1 (DVS.Vector Word16) where
  unmatchedR1 = goDVSR1 0
  {-# INLINE unmatchedR1 #-}

instance UnmatchedR1 (DVS.Vector Word32) where
  unmatchedR1 = goDVSR1 0
  {-# INLINE unmatchedR1 #-}

instance UnmatchedR1 (DVS.Vector Word64) where
  unmatchedR1 = goDVSR1 0
  {-# INLINE unmatchedR1 #-}
