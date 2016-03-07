{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Succinct.RankSelect.Internal
    ( -- * Rank & Select
      Rank0(..)
    , Select0(..)
    , Rank1(..)
    , Select1(..)
    , Rank(..)
    , Select(..)
    ) where

import qualified Data.Vector.Storable           as DVS
import           Data.Word
import           Debug.Trace
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.VectorLike
import           Prelude                        as P
import           Safe

{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

class Rank0 v where
  rank0 :: v -> Position -> Count

class Select0 v where
  select0 :: v -> Count -> Position

class Rank1 v where
  rank1 :: v -> Position -> Count

class Select1 v where
  select1 :: v -> Count -> Position

class Eq a => Rank v a where
  rank :: a -> v -> Position -> Count

class Eq a => Select v a where
  select :: a -> v -> Count -> Position

instance Rank1 Word8 where
  rank1 _ 0  = 0
  rank1 v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (8 - toCount s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x55) + ((r0 .>. 1) .&. 0x55)  in
    let r2 = (r1 .&. 0x33) + ((r1 .>. 2) .&. 0x33)  in
    let r3 = (r2 .&. 0x0f) + ((r2 .>. 4) .&. 0x0f)  in
    let r4 = r3 `mod` 255                           in
    Count $ fromIntegral r4
  {-# INLINABLE rank1 #-}

instance Rank1 Word16 where
  rank1 _ 0  = 0
  rank1 v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (16 - toCount s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x5555) + ((r0 .>. 1) .&. 0x5555)  in
    let r2 = (r1 .&. 0x3333) + ((r1 .>. 2) .&. 0x3333)  in
    let r3 = (r2 .&. 0x0f0f) + ((r2 .>. 4) .&. 0x0f0f)  in
    let r4 = r3 `mod` 255                               in
    Count $ fromIntegral r4
  {-# INLINABLE rank1 #-}

instance Rank1 Word32 where
  rank1 _ 0  = 0
  rank1 v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (32 - toCount s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x55555555) + ((r0 .>. 1) .&. 0x55555555)  in
    let r2 = (r1 .&. 0x33333333) + ((r1 .>. 2) .&. 0x33333333)  in
    let r3 = (r2 .&. 0x0f0f0f0f) + ((r2 .>. 4) .&. 0x0f0f0f0f)  in
    let r4 = r3 `mod` 255                                       in
    Count $ fromIntegral r4
  {-# INLINABLE rank1 #-}

instance Rank1 Word64 where
  rank1 _ 0  = 0
  rank1 v s0 =
    -- Shift out bits after given position.
    let r0 = v .<. (64 - toCount s0) in
    -- Count set bits in parallel.
    let r1 = (r0 .&. 0x5555555555555555) + ((r0 .>. 1) .&. 0x5555555555555555)  in
    let r2 = (r1 .&. 0x3333333333333333) + ((r1 .>. 2) .&. 0x3333333333333333)  in
    let r3 = (r2 .&. 0x0f0f0f0f0f0f0f0f) + ((r2 .>. 4) .&. 0x0f0f0f0f0f0f0f0f)  in
    let r4 = r3 `mod` 255                                                       in
    Count $ fromIntegral r4
  {-# INLINABLE rank1 #-}

-- TODO: Implement NOT interms of select for word-16
instance Select1 Word8 where
  select1 _ 0 = 0
  select1 v p = select1 (fromIntegral v :: Word16) p
  {-# INLINABLE select1 #-}

-- TODO: Remove redundant code to optimise
instance Select1 Word16 where
  select1 _ 0 = 0
  select1 v rn =
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
  {-# INLINABLE select1 #-}

-- TODO: Remove redundant code to optimise
instance Select1 Word32 where
  select1 _ 0 = 0
  select1 v rn =
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
  {-# INLINABLE select1 #-}

instance Select1 Word64 where
  select1 _ 0 = 0
  select1 v rn =
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
  {-# INLINABLE select1 #-}

rankWords1 :: (Num a, PopCount1 a, Rank1 a, BitLength a) => [a] -> Position -> Count
rankWords1 ws n = if remainder == 0
    then predRank
    else predRank + partRank
  where
    partRank = rank1 r remainder
    remainder = n `mod` endPos
    (ls, rs) = splitAt (fromIntegral $ n `quot` endPos) ws
    predRank = P.sum (map (fromIntegral . popCount1) ls)
    r = headDef 0 rs
    endPos = endPosition (head ws)
{-# INLINABLE rankWords1 #-}

selectWords1 :: (PopCount1 v, Select1 v, BitLength v) => Count -> [v] -> Count -> Position
selectWords1 _ [] (Count r) = Position (fromIntegral r) + 1
selectWords1 n (w:ws) r = if pc < n
    then selectWords1 (n - pc) ws (r + bitLength w)
    else select1 w n + Position (fromIntegral r)
  where
    pc = popCount1 w
{-# INLINABLE selectWords1 #-}

instance Rank1 (DVS.Vector Word8) where
  rank1 v = rankWords1 (DVS.toList v)
  {-# INLINABLE rank1 #-}

instance Select1 (DVS.Vector Word8) where
  select1 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount1 w of
              pc | d <= pc  -> select1 w d + acc
              pc            -> go (n + 1) (d - pc) (acc + 8)
  {-# INLINABLE select1 #-}

rankWords0 :: (Num a, PopCount1 a, Rank0 a, BitLength a) => [a] -> Position -> Count
rankWords0 ws n = if remainder == 0
    then predRank
    else predRank + partRank
  where
    partRank = rank0 r remainder
    remainder = n `mod` endPos
    (ls, rs) = splitAt (fromIntegral $ n `quot` endPos) ws
    predRank = P.sum (map (fromIntegral . popCount1) ls)
    r = headDef 0 rs
    endPos = endPosition (head ws)
{-# INLINABLE rankWords0 #-}

selectWords0 :: (PopCount1 v, Select0 v, BitLength v) => Count -> [v] -> Count -> Position
selectWords0 _ [] (Count r) = Position (fromIntegral r) + 1
selectWords0 n (w:ws) r = if pc < n
    then selectWords0 (n - pc) ws (r + bitLength w)
    else select0 w n + Position (fromIntegral r)
  where
    pc = popCount1 w
{-# INLINABLE selectWords0 #-}

instance Rank0 (DVS.Vector Word8) where
  rank0 v p | p < 1     = 0
            | p < 9     = rank0 (DVS.head v) ((p - 1 `rem` 8) + 1)
            | otherwise = rank0 (DVS.head v) ((p - 1 `rem` 8) + 1) + rank0 (DVS.tail v) (p - 8)
  {-# INLINABLE rank0 #-}

instance Select0 (DVS.Vector Word8) where
  select0 v c = go 0 c 0
    where go _ 0  acc = acc
          go n d acc = let w = (v !!! n) in
            case popCount0 w of
              pc | d <= pc  -> select0 w d + acc
              pc            -> go (n + 1) (d - pc) (acc + 8)
  {-# INLINABLE select0 #-}


-----------------------------------------------------------------------------------

instance Rank0 Word8 where
  rank0 v s0 = toCount s0 - rank1 v s0
  {-# INLINABLE rank0 #-}

instance Rank0 Word16 where
  rank0 v s0 = toCount s0 - rank1 v s0
  {-# INLINABLE rank0 #-}

instance Rank0 Word32 where
  rank0 v s0 = toCount s0 - rank1 v s0
  {-# INLINABLE rank0 #-}

instance Rank0 Word64 where
  rank0 v s0 = toCount s0 - rank1 v s0
  {-# INLINABLE rank0 #-}

-- TODO: Implement NOT interms of select for word-16
instance Select0 Word8 where
  select0 v = select1 (comp v)
  {-# INLINABLE select0 #-}

instance Select0 Word16 where
  select0 v = select1 (comp v)
  {-# INLINABLE select0 #-}

instance Select0 Word32 where
  select0 v = select1 (comp v)
  {-# INLINABLE select0 #-}

instance Select0 Word64 where
  select0 v = select1 (comp v)
  {-# INLINABLE select0 #-}

-----------------------------------------------------------------------------------

instance Rank0 [Bool] where
  rank0 = go 0
    where go r _ 0 = r
          go r (False:bs) p = go (r + 1) bs (p - 1)
          go r (True:bs) p  = go  r      bs (p - 1)
          go _ [] _         = error "Out of range"
  {-# INLINABLE rank0 #-}

instance Select0 [Bool] where
  select0 = go 0
    where go r _ 0 = r
          go r (False:bs) c = go (r + 1) bs (c - 1)
          go r (True:bs)  c = go (r + 1) bs  c
          go _ []         _ = error "Out of range"
  {-# INLINABLE select0 #-}

instance Rank1 [Bool] where
  rank1 = go 0
    where go r _ 0 = r
          go r (True :bs) p = go (r + 1) bs (p - 1)
          go r (False:bs) p = go  r      bs (p - 1)
          go _ [] _         = error "Out of range"
  {-# INLINABLE rank1 #-}

instance Select1 [Bool] where
  select1 = go 0
    where go r _ 0 = r
          go r (True :bs) c = go (r + 1) bs (c - 1)
          go r (False:bs) c = go (r + 1) bs  c
          go _ []         _ = error "Out of range"
  {-# INLINABLE select1 #-}

instance Rank [Bool] Bool where
  rank a = if a then rank1 else rank0
  {-# INLINABLE rank #-}

instance Select [Bool] Bool where
  select a = if a then select1 else select0
  {-# INLINABLE select #-}
