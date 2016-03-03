{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Succinct.RankSelect.Internal
    ( -- * Rank & Select
      BitRank(..)
    , BitSelect(..)
    , Rank(..)
    , Select(..)
    ) where

import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning

class BitRank v where
  bitRank :: v -> Position -> Count

class BitSelect v where
  bitSelect :: v -> Count -> Position

class Eq a => Rank v a where
  rank :: a -> v -> Position -> Count

class Eq a => Select v a where
  select :: a -> v -> Count -> Position

instance BitRank Word8 where
  bitRank v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (8 - toCount s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x55) + ((r0 .>. 1) .&. 0x55)  in
    let r2 = (r1 .&. 0x33) + ((r1 .>. 2) .&. 0x33)  in
    let r3 = (r2 .&. 0x0f) + ((r2 .>. 4) .&. 0x0f)  in
    let r4 = r3 `mod` 255                           in
    Count $ fromIntegral r4
  {-# INLINABLE bitRank #-}

instance BitRank Word16 where
  bitRank v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (16 - toCount s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x5555) + ((r0 .>. 1) .&. 0x5555)  in
    let r2 = (r1 .&. 0x3333) + ((r1 .>. 2) .&. 0x3333)  in
    let r3 = (r2 .&. 0x0f0f) + ((r2 .>. 4) .&. 0x0f0f)  in
    let r4 = r3 `mod` 255                               in
    Count $ fromIntegral r4
  {-# INLINABLE bitRank #-}

instance BitRank Word32 where
  bitRank v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (32 - toCount s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x55555555) + ((r0 .>. 1) .&. 0x55555555)  in
    let r2 = (r1 .&. 0x33333333) + ((r1 .>. 2) .&. 0x33333333)  in
    let r3 = (r2 .&. 0x0f0f0f0f) + ((r2 .>. 4) .&. 0x0f0f0f0f)  in
    let r4 = r3 `mod` 255                                       in
    Count $ fromIntegral r4
  {-# INLINABLE bitRank #-}

instance BitRank Word64 where
  bitRank v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (64 - toCount s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x5555555555555555) + ((r0 .>. 1) .&. 0x5555555555555555)  in
    let r2 = (r1 .&. 0x3333333333333333) + ((r1 .>. 2) .&. 0x3333333333333333)  in
    let r3 = (r2 .&. 0x0f0f0f0f0f0f0f0f) + ((r2 .>. 4) .&. 0x0f0f0f0f0f0f0f0f)  in
    let r4 = r3 `mod` 255                                                       in
    Count $ fromIntegral r4
  {-# INLINABLE bitRank #-}

-- TODO: Implement NOT interms of select for word-16
instance BitSelect Word8 where
  bitSelect v = bitSelect (fromIntegral v :: Word16)
  {-# INLINABLE bitSelect #-}

-- TODO: Remove redundant code to optimise
instance BitSelect Word16 where
  bitSelect v rn =
    -- Do a normal parallel bit count for a 64-bit integer,
    -- but store all intermediate steps.
    let a = (v .&. 0x5555) + ((v .>.  1) .&. 0x5555)    in
    let b = (a .&. 0x3333) + ((a .>.  2) .&. 0x3333)    in
    let c = (b .&. 0x0f0f) + ((b .>.  4) .&. 0x0f0f)    in
    let d = (c .&. 0x00ff) + ((c .>.  8) .&. 0x00ff)    in
    -- Now do branchless select!
    let r0 = d + 1 - (fromIntegral (getCount rn) :: Word16)                     in
    let s0 = 64 :: Word16                                                       in
    let t0 = (d .>. 32) + (d .>. 48)                                            in
    let s1 = s0 - ((t0 - r0) .&. 256) .>. 3                                     in
    let r1 = r0 - (t0 .&. ((t0 - r0) .>. 8))                                    in
    let t1 =      (d .>. fromIntegral (s1 - 16)) .&. 0xff                       in
    let s2 = s1 - ((t1 - r1) .&. 256) .>. 4                                     in
    let r2 = r1 - (t1 .&. ((t1 - r1) .>. 8))                                    in
    let t2 =      (c .>. fromIntegral (s2 - 8))  .&. 0xf                        in
    let s3 = s2 - ((t2 - r2) .&. 256) .>. 5                                     in
    let r3 = r2 - (t2 .&. ((t2 - r2) .>. 8))                                    in
    let t3 =      (b .>. fromIntegral (s3 - 4))  .&. 0x7                        in
    let s4 = s3 - ((t3 - r3) .&. 256) .>. 6                                     in
    let r4 = r3 - (t3 .&. ((t3 - r3) .>. 8))                                    in
    let t4 =      (a .>. fromIntegral (s4 - 2))  .&. 0x3                        in
    let s5 = s4 - ((t4 - r4) .&. 256) .>. 7                                     in
    let r5 = r4 - (t4 .&. ((t4 - r4) .>. 8))                                    in
    let t5 =      (v .>. fromIntegral (s5 - 1))  .&. 0x1                        in
    let s6 = s5 - ((t5 - r5) .&. 256) .>. 8                                     in
    fromIntegral s6
  {-# INLINABLE bitSelect #-}

-- TODO: Remove redundant code to optimise
instance BitSelect Word32 where
  bitSelect v rn =
    -- Do a normal parallel bit count for a 64-bit integer,
    -- but store all intermediate steps.
    let a = (v .&. 0x55555555) + ((v .>.  1) .&. 0x55555555)    in
    let b = (a .&. 0x33333333) + ((a .>.  2) .&. 0x33333333)    in
    let c = (b .&. 0x0f0f0f0f) + ((b .>.  4) .&. 0x0f0f0f0f)    in
    let d = (c .&. 0x00ff00ff) + ((c .>.  8) .&. 0x00ff00ff)    in
    let e = (d .&. 0x000000ff) + ((d .>. 16) .&. 0x000000ff)    in
    -- Now do branchless select!
    let r0 = e + 1 - (fromIntegral (getCount rn) :: Word32)                     in
    let s0 = 64 :: Word32                                                       in
    let t0 = (d .>. 32) + (d .>. 48)                                            in
    let s1 = s0 - ((t0 - r0) .&. 256) .>. 3                                     in
    let r1 = r0 - (t0 .&. ((t0 - r0) .>. 8))                                    in
    let t1 =      (d .>. fromIntegral (s1 - 16)) .&. 0xff                       in
    let s2 = s1 - ((t1 - r1) .&. 256) .>. 4                                     in
    let r2 = r1 - (t1 .&. ((t1 - r1) .>. 8))                                    in
    let t2 =      (c .>. fromIntegral (s2 - 8))  .&. 0xf                        in
    let s3 = s2 - ((t2 - r2) .&. 256) .>. 5                                     in
    let r3 = r2 - (t2 .&. ((t2 - r2) .>. 8))                                    in
    let t3 =      (b .>. fromIntegral (s3 - 4))  .&. 0x7                        in
    let s4 = s3 - ((t3 - r3) .&. 256) .>. 6                                     in
    let r4 = r3 - (t3 .&. ((t3 - r3) .>. 8))                                    in
    let t4 =      (a .>. fromIntegral (s4 - 2))  .&. 0x3                        in
    let s5 = s4 - ((t4 - r4) .&. 256) .>. 7                                     in
    let r5 = r4 - (t4 .&. ((t4 - r4) .>. 8))                                    in
    let t5 =      (v .>. fromIntegral (s5 - 1))  .&. 0x1                        in
    let s6 = s5 - ((t5 - r5) .&. 256) .>. 8                                     in
    fromIntegral s6
  {-# INLINABLE bitSelect #-}

instance BitSelect Word64 where
  bitSelect v rn =
    -- Do a normal parallel bit count for a 64-bit integer,
    -- but store all intermediate steps.
    let a = (v .&. 0x5555555555555555) + ((v .>.  1) .&. 0x5555555555555555)    in
    let b = (a .&. 0x3333333333333333) + ((a .>.  2) .&. 0x3333333333333333)    in
    let c = (b .&. 0x0f0f0f0f0f0f0f0f) + ((b .>.  4) .&. 0x0f0f0f0f0f0f0f0f)    in
    let d = (c .&. 0x00ff00ff00ff00ff) + ((c .>.  8) .&. 0x00ff00ff00ff00ff)    in
    -- Now do branchless select!
    let r0 = fromIntegral (getCount rn) :: Word64                               in
    let s0 = 64 :: Word64                                                       in
    let t0 = (d .>. 32) + (d .>. 48)                                            in
    let s1 = s0 - ((t0 - r0) .&. 256) .>. 3                                     in
    let r1 = r0 - (t0 .&. ((t0 - r0) .>. 8))                                    in
    let t1 =      (d .>. fromIntegral (s1 - 16)) .&. 0xff                       in
    let s2 = s1 - ((t1 - r1) .&. 256) .>. 4                                     in
    let r2 = r1 - (t1 .&. ((t1 - r1) .>. 8))                                    in
    let t2 =      (c .>. fromIntegral (s2 - 8))  .&. 0xf                        in
    let s3 = s2 - ((t2 - r2) .&. 256) .>. 5                                     in
    let r3 = r2 - (t2 .&. ((t2 - r2) .>. 8))                                    in
    let t3 =      (b .>. fromIntegral (s3 - 4))  .&. 0x7                        in
    let s4 = s3 - ((t3 - r3) .&. 256) .>. 6                                     in
    let r4 = r3 - (t3 .&. ((t3 - r3) .>. 8))                                    in
    let t4 =      (a .>. fromIntegral (s4 - 2))  .&. 0x3                        in
    let s5 = s4 - ((t4 - r4) .&. 256) .>. 7                                     in
    let r5 = r4 - (t4 .&. ((t4 - r4) .>. 8))                                    in
    let t5 =      (v .>. fromIntegral (s5 - 1))  .&. 0x1                        in
    let s6 = s5 - ((t5 - r5) .&. 256) .>. 8                                     in
    let s7 =      65 - s6                                                       in
    fromIntegral s7
  {-# INLINABLE bitSelect #-}

instance Rank [Bool] Bool where
  rank a = go 0
    where go r _ 0 = r
          go r (b:bs) p = go (if b == a then r + 1 else r) bs (p - 1)
          go _ [] _         = error "Out of range"
  {-# INLINABLE rank #-}

instance Select [Bool] Bool where
  select a = go 0
    where go r _ 0 = r
          go r (b:bs)  c = go (r + 1) bs (if a == b then c - 1 else c)
          go _ []         _ = error "Out of range"
  {-# INLINABLE select #-}
