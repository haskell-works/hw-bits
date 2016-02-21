{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE TypeFamilies               #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Succinct.Internal
    ( -- * Count & Positioning
      Count(..)
    , Position(..)
      -- * Rank & Select
    , BeBitRank(..)
    , BeBitSelect(..)
    , BitRank(..)
    , BitSelect(..)
    , LeBitRank(..)
    , LeBitSelect(..)
    , Rank(..)
    , Select(..)
      -- * Nearest Neighbour Dictionary
    , bitPred
    , bitSucc
      -- * Balanced Parentheses
    , BalancedParens(..)
      -- * Bit map
    , BitLength(..)
    , BitWise(..)
    , PopCount(..)
    , Shift(..)
    , TestBit(..)
      -- * Bit manipulation
    , Broadword(..)
    ) where

import qualified Data.Bits as B
import           Data.Int
import           Data.Word

-- We pervasively use precedence to avoid excessive parentheses, and we use
-- the same precedence conventions of the C programming language: arithmetic
-- operators come first, ordered in the standard way, followed by shifts,
-- followed by logical operators; ⊕ sits between | and &.
infixl 9 .?.
infixl 8 .<., .>.
infixl 7 .&.        -- Bitwise AND.  eg. ∧
infixl 6 .^.        -- Bitwise XOR.  eg. ⊕
infixl 5 .|.        -- Bitwise OR.   eg. ∨

newtype Count = Count { getCount :: Word64 }
  deriving (Eq, Num, Ord, Enum, Real, Integral, Show)

newtype Position = Position { getPosition :: Int64 }
  deriving (Eq, Num, Ord, Enum, Real, Integral, Show)

class BeBitRank v where
  beBitRank :: v -> Position -> Count

class BeBitSelect v where
  beBitSelect :: v -> Count -> Position

class BitRank v where
  bitRank :: v -> Position -> Count

class BitSelect v where
  bitSelect :: v -> Count -> Position

class LeBitRank v where
  leBitRank :: v -> Position -> Count

class LeBitSelect v where
  leBitSelect :: v -> Count -> Position

class Rank v a where
  rank :: Eq a => Position -> a -> v -> Count

class Select v where
  select :: Eq a => Count -> v -> a -> Position

class BitLength v where
  bitLength :: v -> Count

  endPosition :: v -> Position
  endPosition = Position . fromIntegral . getCount . bitLength

class Shift a where
  (.<.) :: a -> Int64 -> a
  (.>.) :: a -> Int64 -> a

class TestBit a where
  (.?.) :: a -> Position -> Bool

class BitWise a where
  (.&.) :: a -> a -> a
  (.|.) :: a -> a -> a
  (.^.) :: a -> a -> a
  comp  :: a -> a

class PopCount a where
  popCount :: a -> Count

class Broadword a where
  bwL8 :: a
  bwH8 :: a

class BalancedParens v where
  findOpen :: v -> Position -> Position
  findClose :: v -> Position -> Position
  enclose :: v -> Position -> Position

--------------------------------------------------------------------------------
-- Nearest Neighbour Dictionary

bitPred :: (BitRank v, BitSelect v) => v -> Position -> Position
bitPred v p = bitSelect v (bitRank v p - 1)

bitSucc :: (BitRank v, BitSelect v) => v -> Position -> Position
bitSucc v p = bitSelect v (bitRank v p + 1)

--------------------------------------------------------------------------------
-- Instances

instance Broadword Word8 where
  bwL8 = 0x01
  {-# INLINABLE bwL8 #-}

  bwH8 = 0x80
  {-# INLINABLE bwH8 #-}

instance Broadword Word16 where
  bwL8 = 0x0101
  {-# INLINABLE bwL8 #-}

  bwH8 = 0x8080
  {-# INLINABLE bwH8 #-}

instance Broadword Word32 where
  bwL8 = 0x01010101
  {-# INLINABLE bwL8 #-}

  bwH8 = 0x80808080
  {-# INLINABLE bwH8 #-}

instance Broadword Word64 where
  bwL8 = 0x0101010101010101
  {-# INLINABLE bwL8 #-}

  bwH8 = 0x8080808080808080
  {-# INLINABLE bwH8 #-}

instance BitLength Word8 where
  bitLength _ = 8
  {-# INLINABLE bitLength #-}

instance BitLength Word16 where
  bitLength _ = 16
  {-# INLINABLE bitLength #-}

instance BitLength Word32 where
  bitLength _ = 32
  {-# INLINABLE bitLength #-}

instance BitLength Word64 where
  bitLength _ = 64
  {-# INLINABLE bitLength #-}

instance TestBit Word8 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance TestBit Word16 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance TestBit Word32 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance TestBit Word64 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance PopCount Word8 where
  popCount x0 = Count (fromIntegral x3)
    where
      x1 = x0 - ((x0 .&. 0xaa) .>. 1)
      x2 = (x1 .&. 0x33) + ((x1 .>. 2) .&. 0x33)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f
  {-# INLINABLE popCount #-}

instance PopCount Word16 where
  popCount x0 = Count (fromIntegral ((x3 * 0x0101) .>. 8))
    where
      x1 = x0 - ((x0 .&. 0xaaaa) .>. 1)
      x2 = (x1 .&. 0x3333) + ((x1 .>. 2) .&. 0x3333)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f0f
  {-# INLINABLE popCount #-}

instance PopCount Word32 where
  popCount x0 = Count (fromIntegral ((x3 * 0x01010101) .>. 24))
    where
      x1 = x0 - ((x0 .&. 0xaaaaaaaa) .>. 1)
      x2 = (x1 .&. 0x33333333) + ((x1 .>. 2) .&. 0x33333333)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f0f0f0f
  {-# INLINABLE popCount #-}

instance PopCount Word64 where
  popCount x0 = Count ((x3 * 0x0101010101010101) .>. 56)
    where
      x1 = x0 - ((x0 .&. 0xaaaaaaaaaaaaaaaa) .>. 1)
      x2 = (x1 .&. 0x3333333333333333) + ((x1 .>. 2) .&. 0x3333333333333333)
      x3 = (x2 + (x2 .>. 4)) .&. 0x0f0f0f0f0f0f0f0f
  {-# INLINABLE popCount #-}

instance BitWise Word8 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

instance BitWise Word16 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

instance BitWise Word32 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

instance BitWise Word64 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

instance Shift Word8  where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}

instance Shift Word16 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}

instance Shift Word32 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}

instance Shift Word64 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}

instance LeBitRank Word8 where
  leBitRank v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (8 - getPosition s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x55) + ((r0 .>. 1) .&. 0x55)  in
    let r2 = (r1 .&. 0x33) + ((r1 .>. 2) .&. 0x33)  in
    let r3 = (r2 .&. 0x0f) + ((r2 .>. 4) .&. 0x0f)  in
    let r4 = r3 `mod` 255                           in
    Count $ fromIntegral r4
  {-# INLINABLE leBitRank #-}

instance LeBitRank Word16 where
  leBitRank v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (16 - getPosition s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x5555) + ((r0 .>. 1) .&. 0x5555)  in
    let r2 = (r1 .&. 0x3333) + ((r1 .>. 2) .&. 0x3333)  in
    let r3 = (r2 .&. 0x0f0f) + ((r2 .>. 4) .&. 0x0f0f)  in
    let r4 = r3 `mod` 255                               in
    Count $ fromIntegral r4
  {-# INLINABLE leBitRank #-}

instance LeBitRank Word32 where
  leBitRank v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (32 - getPosition s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x55555555) + ((r0 .>. 1) .&. 0x55555555)  in
    let r2 = (r1 .&. 0x33333333) + ((r1 .>. 2) .&. 0x33333333)  in
    let r3 = (r2 .&. 0x0f0f0f0f) + ((r2 .>. 4) .&. 0x0f0f0f0f)  in
    let r4 = r3 `mod` 255                                       in
    Count $ fromIntegral r4
  {-# INLINABLE leBitRank #-}

instance LeBitRank Word64 where
  leBitRank v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (64 - getPosition s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x5555555555555555) + ((r0 .>. 1) .&. 0x5555555555555555)  in
    let r2 = (r1 .&. 0x3333333333333333) + ((r1 .>. 2) .&. 0x3333333333333333)  in
    let r3 = (r2 .&. 0x0f0f0f0f0f0f0f0f) + ((r2 .>. 4) .&. 0x0f0f0f0f0f0f0f0f)  in
    let r4 = r3 `mod` 255                                                       in
    Count $ fromIntegral r4
  {-# INLINABLE leBitRank #-}

-- TODO: Implement NOT interms of select for word-16
instance LeBitSelect Word8 where
  leBitSelect v = leBitSelect (fromIntegral v :: Word16)
  {-# INLINABLE leBitSelect #-}

-- TODO: Remove redundant code to optimise
instance LeBitSelect Word16 where
  leBitSelect v rn =
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
  {-# INLINABLE leBitSelect #-}

-- TODO: Remove redundant code to optimise
instance LeBitSelect Word32 where
  leBitSelect v rn =
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
  {-# INLINABLE leBitSelect #-}

instance LeBitSelect Word64 where
  leBitSelect v rn =
    -- Do a normal parallel bit count for a 64-bit integer,
    -- but store all intermediate steps.
    let a = (v .&. 0x5555555555555555) + ((v .>.  1) .&. 0x5555555555555555)    in
    let b = (a .&. 0x3333333333333333) + ((a .>.  2) .&. 0x3333333333333333)    in
    let c = (b .&. 0x0f0f0f0f0f0f0f0f) + ((b .>.  4) .&. 0x0f0f0f0f0f0f0f0f)    in
    let d = (c .&. 0x00ff00ff00ff00ff) + ((c .>.  8) .&. 0x00ff00ff00ff00ff)    in
    let e = (d .&. 0x000000ff000000ff) + ((d .>. 16) .&. 0x000000ff000000ff)    in
    let f = (e .&. 0x00000000000000ff) + ((e .>. 32) .&. 0x00000000000000ff)    in
    -- Now do branchless select!
    let r0 = f + 1 - (fromIntegral (getCount rn) :: Word64)                     in
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
  {-# INLINABLE leBitSelect #-}

instance BeBitRank Word8 where
  beBitRank v s0 =
    -- Shift out bits after given position.
    let r0 = v .>. (8 - getPosition s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x55) + ((r0 .>. 1) .&. 0x55)  in
    let r2 = (r1 .&. 0x33) + ((r1 .>. 2) .&. 0x33)  in
    let r3 = (r2 .&. 0x0f) + ((r2 .>. 4) .&. 0x0f)  in
    let r4 = r3 `mod` 255                           in
    Count $ fromIntegral r4
  {-# INLINABLE beBitRank #-}

instance BeBitRank Word16 where
  beBitRank v s0 =
    -- Shift out bits after given position.
    let r0 = v .>. (16 - getPosition s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x5555) + ((r0 .>. 1) .&. 0x5555)  in
    let r2 = (r1 .&. 0x3333) + ((r1 .>. 2) .&. 0x3333)  in
    let r3 = (r2 .&. 0x0f0f) + ((r2 .>. 4) .&. 0x0f0f)  in
    let r4 = r3 `mod` 255                               in
    Count $ fromIntegral r4
  {-# INLINABLE beBitRank #-}

instance BeBitRank Word32 where
  beBitRank v s0 =
    -- Shift out bits after given position.
    let r0 = v .>. (32 - getPosition s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x55555555) + ((r0 .>. 1) .&. 0x55555555)  in
    let r2 = (r1 .&. 0x33333333) + ((r1 .>. 2) .&. 0x33333333)  in
    let r3 = (r2 .&. 0x0f0f0f0f) + ((r2 .>. 4) .&. 0x0f0f0f0f)  in
    let r4 = r3 `mod` 255                                       in
    Count $ fromIntegral r4
  {-# INLINABLE beBitRank #-}

instance BeBitRank Word64 where
  beBitRank v s0 =
    -- let s = fromIntegral s0 :: Word64 in
    -- Shift out bits after given position.
    let r0 = v .>. (64 - getPosition s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x5555555555555555) + ((r0 .>. 1) .&. 0x5555555555555555)  in
    let r2 = (r1 .&. 0x3333333333333333) + ((r1 .>. 2) .&. 0x3333333333333333)  in
    let r3 = (r2 .&. 0x0f0f0f0f0f0f0f0f) + ((r2 .>. 4) .&. 0x0f0f0f0f0f0f0f0f)  in
    let r4 = r3 `mod` 255                                                       in
    Count $ fromIntegral r4
  {-# INLINABLE beBitRank #-}

instance BeBitSelect Word64 where
  beBitSelect v rn =
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
  {-# INLINABLE beBitSelect #-}

instance BitRank Word8 where
  bitRank = leBitRank
  {-# INLINABLE bitRank #-}

instance BitRank Word16 where
  bitRank = leBitRank
  {-# INLINABLE bitRank #-}

instance BitRank Word32 where
  bitRank = leBitRank
  {-# INLINABLE bitRank #-}

instance BitRank Word64 where
  bitRank = leBitRank
  {-# INLINABLE bitRank #-}

instance BitSelect Word8 where
  bitSelect = leBitSelect
  {-# INLINABLE bitSelect #-}

instance BitSelect Word16 where
  bitSelect = leBitSelect
  {-# INLINABLE bitSelect #-}

instance BitSelect Word32 where
  bitSelect = leBitSelect
  {-# INLINABLE bitSelect #-}

instance BitSelect Word64 where
  bitSelect = leBitSelect
  {-# INLINABLE bitSelect #-}
