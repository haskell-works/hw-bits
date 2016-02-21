{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Succinct.Internal
    ( -- * Rank & Select
      BeBitRank(..)
    , BeBitSelect(..)
    , BitLength(..)
    , BitRank(..)
    , BitSelect(..)
    , BitWise(..)
    , Broadword(..)
    , LeBitRank(..)
    , LeBitSelect(..)
    , PopCount(..)
    , Rank(..)
    , Select(..)
    , Shift(..)
    , TestBit(..)
    ) where

import qualified Data.Bits as B
import           Data.Int
import           Data.Word

infixl 9 .?.
infixl 8 .<., .>.
infixl 7 .&.
infixl 6 .^.
infixl 5 .|.

class BeBitRank v where
  beBitRank :: Int64 -> v -> Int64

class BeBitSelect v where
  beBitSelect :: Int64 -> v -> Int64

class BitLength v where
  bitLength :: v -> Int64

class BitRank v where
  bitRank :: Int64 -> v -> Int64

class BitSelect v where
  bitSelect :: Int64 -> v -> Int64

class LeBitRank v where
  leBitRank :: Int64 -> v -> Int64

class LeBitSelect v where
  leBitSelect :: Int64 -> v -> Int64

class Rank v a where
  rank :: Eq a => Int64 -> a -> v -> Int64

class Select v where
  select :: Eq a => Int64 -> v -> a -> Int64

class Shift a where
  (.<.) :: a -> Int64 -> a
  (.>.) :: a -> Int64 -> a

class TestBit a where
  (.?.) :: a -> Int64 -> Bool

class BitWise a where
  (.&.) :: a -> a -> a
  (.|.) :: a -> a -> a
  (.^.) :: a -> a -> a
  comp  :: a -> a

class PopCount a where
  popCount :: a -> Int64

class Broadword a where
  bwL8 :: a
  bwH8 :: a

instance Broadword Word8 where
  bwL8 = 0x01
  bwH8 = 0x80

instance Broadword Word16 where
  bwL8 = 0x0101
  bwH8 = 0x8080

instance Broadword Word32 where
  bwL8 = 0x01010101
  bwH8 = 0x80808080

instance Broadword Word64 where
  bwL8 = 0x0101010101010101
  bwH8 = 0x8080808080808080

instance BitLength Word8 where
  bitLength _ = 8

instance BitLength Word16 where
  bitLength _ = 16

instance BitLength Word32 where
  bitLength _ = 32

instance BitLength Word64 where
  bitLength _ = 64

instance TestBit Word8 where
  (.?.) w n = B.testBit w (fromIntegral n)

instance TestBit Word16 where
  (.?.) w n = B.testBit w (fromIntegral n)

instance TestBit Word32 where
  (.?.) w n = B.testBit w (fromIntegral n)

instance TestBit Word64 where
  (.?.) w n = B.testBit w (fromIntegral n)

instance PopCount Word8 where
  popCount = fromIntegral . B.popCount

instance PopCount Word16 where
  popCount = fromIntegral . B.popCount

instance PopCount Word32 where
  popCount = fromIntegral . B.popCount

instance PopCount Word64 where
  popCount = fromIntegral . B.popCount

instance BitWise Word8 where
  (.&.) = (B..&.)
  (.|.) = (B..|.)
  (.^.) = B.xor
  comp  = B.complement

instance BitWise Word16 where
  (.&.) = (B..&.)
  (.|.) = (B..|.)
  (.^.) = B.xor
  comp  = B.complement

instance BitWise Word32 where
  (.&.) = (B..&.)
  (.|.) = (B..|.)
  (.^.) = B.xor
  comp  = B.complement

instance BitWise Word64 where
  (.&.) = (B..&.)
  (.|.) = (B..|.)
  (.^.) = B.xor
  comp  = B.complement

instance Shift Word8  where
  (.<.) w n = B.shiftL w (fromIntegral n)
  (.>.) w n = B.shiftR w (fromIntegral n)

instance Shift Word16 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  (.>.) w n = B.shiftR w (fromIntegral n)

instance Shift Word32 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  (.>.) w n = B.shiftR w (fromIntegral n)

instance Shift Word64 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  (.>.) w n = B.shiftR w (fromIntegral n)

instance LeBitRank Word8 where
  leBitRank s0 v =
    -- Shift out bits after given position.
    let r0 = v .<. (8 - s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x55) + ((r0 .>. 1) .&. 0x55)  in
    let r2 = (r1 .&. 0x33) + ((r1 .>. 2) .&. 0x33)  in
    let r3 = (r2 .&. 0x0f) + ((r2 .>. 4) .&. 0x0f)  in
    let r4 = r3 `mod` 255                           in
    fromIntegral r4 :: Int64

instance LeBitRank Word16 where
  leBitRank s0 v =
    -- Shift out bits after given position.
    let r0 = v .<. (16 - s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x5555) + ((r0 .>. 1) .&. 0x5555)  in
    let r2 = (r1 .&. 0x3333) + ((r1 .>. 2) .&. 0x3333)  in
    let r3 = (r2 .&. 0x0f0f) + ((r2 .>. 4) .&. 0x0f0f)  in
    let r4 = r3 `mod` 255                               in
    fromIntegral r4 :: Int64

instance LeBitRank Word32 where
  leBitRank s0 v =
    -- Shift out bits after given position.
    let r0 = v .<. (32 - s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x55555555) + ((r0 .>. 1) .&. 0x55555555)  in
    let r2 = (r1 .&. 0x33333333) + ((r1 .>. 2) .&. 0x33333333)  in
    let r3 = (r2 .&. 0x0f0f0f0f) + ((r2 .>. 4) .&. 0x0f0f0f0f)  in
    let r4 = r3 `mod` 255                                       in
    fromIntegral r4 :: Int64

instance LeBitRank Word64 where
  leBitRank s0 v =
    -- Shift out bits after given position.
    let r0 = v .<. (64 - s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x5555555555555555) + ((r0 .>. 1) .&. 0x5555555555555555)  in
    let r2 = (r1 .&. 0x3333333333333333) + ((r1 .>. 2) .&. 0x3333333333333333)  in
    let r3 = (r2 .&. 0x0f0f0f0f0f0f0f0f) + ((r2 .>. 4) .&. 0x0f0f0f0f0f0f0f0f)  in
    let r4 = r3 `mod` 255                                                       in
    fromIntegral r4 :: Int64

-- TODO: Implement NOT interms of select for word-16
instance LeBitSelect Word8 where
  leBitSelect rn v = leBitSelect rn (fromIntegral v :: Word16)

-- TODO: Remove redundant code to optimise
instance LeBitSelect Word16 where
  leBitSelect rn v =
    -- Do a normal parallel bit count for a 64-bit integer,
    -- but store all intermediate steps.
    let a = (v .&. 0x5555) + ((v .>.  1) .&. 0x5555)    in
    let b = (a .&. 0x3333) + ((a .>.  2) .&. 0x3333)    in
    let c = (b .&. 0x0f0f) + ((b .>.  4) .&. 0x0f0f)    in
    let d = (c .&. 0x00ff) + ((c .>.  8) .&. 0x00ff)    in
    -- Now do branchless select!
    let r0 = d + 1 - (fromIntegral rn :: Word16)                                in
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

-- TODO: Remove redundant code to optimise
instance LeBitSelect Word32 where
  leBitSelect rn v =
    -- Do a normal parallel bit count for a 64-bit integer,
    -- but store all intermediate steps.
    let a = (v .&. 0x55555555) + ((v .>.  1) .&. 0x55555555)    in
    let b = (a .&. 0x33333333) + ((a .>.  2) .&. 0x33333333)    in
    let c = (b .&. 0x0f0f0f0f) + ((b .>.  4) .&. 0x0f0f0f0f)    in
    let d = (c .&. 0x00ff00ff) + ((c .>.  8) .&. 0x00ff00ff)    in
    let e = (d .&. 0x000000ff) + ((d .>. 16) .&. 0x000000ff)    in
    -- Now do branchless select!
    let r0 = e + 1 - (fromIntegral rn :: Word32)                                in
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

instance LeBitSelect Word64 where
  leBitSelect rn v =
    -- Do a normal parallel bit count for a 64-bit integer,
    -- but store all intermediate steps.
    let a = (v .&. 0x5555555555555555) + ((v .>.  1) .&. 0x5555555555555555)    in
    let b = (a .&. 0x3333333333333333) + ((a .>.  2) .&. 0x3333333333333333)    in
    let c = (b .&. 0x0f0f0f0f0f0f0f0f) + ((b .>.  4) .&. 0x0f0f0f0f0f0f0f0f)    in
    let d = (c .&. 0x00ff00ff00ff00ff) + ((c .>.  8) .&. 0x00ff00ff00ff00ff)    in
    let e = (d .&. 0x000000ff000000ff) + ((d .>. 16) .&. 0x000000ff000000ff)    in
    let f = (e .&. 0x00000000000000ff) + ((e .>. 32) .&. 0x00000000000000ff)    in
    -- Now do branchless select!
    let r0 = f + 1 - (fromIntegral rn :: Word64)                                in
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
    fromIntegral s6

instance BeBitRank Word8 where
  beBitRank s0 v =
    -- Shift out bits after given position.
    let r0 = v .>. (8 - s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x55) + ((r0 .>. 1) .&. 0x55)  in
    let r2 = (r1 .&. 0x33) + ((r1 .>. 2) .&. 0x33)  in
    let r3 = (r2 .&. 0x0f) + ((r2 .>. 4) .&. 0x0f)  in
    let r4 = r3 `mod` 255                           in
    fromIntegral r4 :: Int64

instance BeBitRank Word16 where
  beBitRank s0 v =
    -- Shift out bits after given position.
    let r0 = v .>. (16 - s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x5555) + ((r0 .>. 1) .&. 0x5555)  in
    let r2 = (r1 .&. 0x3333) + ((r1 .>. 2) .&. 0x3333)  in
    let r3 = (r2 .&. 0x0f0f) + ((r2 .>. 4) .&. 0x0f0f)  in
    let r4 = r3 `mod` 255                               in
    fromIntegral r4 :: Int64

instance BeBitRank Word32 where
  beBitRank s0 v =
    -- Shift out bits after given position.
    let r0 = v .>. (32 - s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x55555555) + ((r0 .>. 1) .&. 0x55555555)  in
    let r2 = (r1 .&. 0x33333333) + ((r1 .>. 2) .&. 0x33333333)  in
    let r3 = (r2 .&. 0x0f0f0f0f) + ((r2 .>. 4) .&. 0x0f0f0f0f)  in
    let r4 = r3 `mod` 255                                       in
    fromIntegral r4 :: Int64

instance BeBitRank Word64 where
  beBitRank s0 v =
    -- let s = fromIntegral s0 :: Word64 in
    -- Shift out bits after given position.
    let r0 = v .>. (64 - s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x5555555555555555) + ((r0 .>. 1) .&. 0x5555555555555555)  in
    let r2 = (r1 .&. 0x3333333333333333) + ((r1 .>. 2) .&. 0x3333333333333333)  in
    let r3 = (r2 .&. 0x0f0f0f0f0f0f0f0f) + ((r2 .>. 4) .&. 0x0f0f0f0f0f0f0f0f)  in
    let r4 = r3 `mod` 255                                                       in
    fromIntegral r4 :: Int64


instance BeBitSelect Word64 where
  beBitSelect rn v =
    -- Do a normal parallel bit count for a 64-bit integer,
    -- but store all intermediate steps.
    let a = (v .&. 0x5555555555555555) + ((v .>.  1) .&. 0x5555555555555555)    in
    let b = (a .&. 0x3333333333333333) + ((a .>.  2) .&. 0x3333333333333333)    in
    let c = (b .&. 0x0f0f0f0f0f0f0f0f) + ((b .>.  4) .&. 0x0f0f0f0f0f0f0f0f)    in
    let d = (c .&. 0x00ff00ff00ff00ff) + ((c .>.  8) .&. 0x00ff00ff00ff00ff)    in
    -- Now do branchless select!
    let r0 = fromIntegral rn :: Word64                                          in
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

instance BitRank Word8 where
  bitRank = leBitRank

instance BitRank Word16 where
  bitRank = leBitRank

instance BitRank Word32 where
  bitRank = leBitRank

instance BitRank Word64 where
  bitRank = leBitRank

instance BitSelect Word8 where
  bitSelect = leBitSelect

instance BitSelect Word16 where
  bitSelect = leBitSelect

instance BitSelect Word32 where
  bitSelect = leBitSelect

instance BitSelect Word64 where
  bitSelect = leBitSelect
