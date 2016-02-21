{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Succinct.Simple
    ( -- * Simple bit vector types
      SimpleBitVector8(..)
    , SimpleBitVector16(..)
    , SimpleBitVector32(..)
    , SimpleBitVector64(..)
    ) where

import           Data.Int
import           Data.Vector
import           Data.Word
import           HaskellWorks.Data.Succinct.Internal
import           Prelude                             as P
import           Safe

newtype SimpleBitVector8  = SimpleBitVector8  (Vector Word8 ) deriving (Eq, Show)
newtype SimpleBitVector16 = SimpleBitVector16 (Vector Word16) deriving (Eq, Show)
newtype SimpleBitVector32 = SimpleBitVector32 (Vector Word32) deriving (Eq, Show)
newtype SimpleBitVector64 = SimpleBitVector64 (Vector Word64) deriving (Eq, Show)

instance TestBit SimpleBitVector8 where
  SimpleBitVector8  v .?. n = case n `quotRem` bitLength (v ! 0) of
    (q, r) -> (v ! fromIntegral q) .?. fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit SimpleBitVector16 where
  SimpleBitVector16 v .?. n = case n `quotRem` bitLength (v ! 0) of
    (q, r) -> (v ! fromIntegral q) .?. fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit SimpleBitVector32 where
  SimpleBitVector32 v .?. n = case n `quotRem` bitLength (v ! 0) of
    (q, r) -> (v ! fromIntegral q) .?. fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit SimpleBitVector64 where
  SimpleBitVector64 v .?. n = case n `quotRem` bitLength (v ! 0) of
    (q, r) -> (v ! fromIntegral q) .?. fromIntegral r
  {-# INLINABLE (.?.) #-}

instance BitRank SimpleBitVector8 where
  bitRank (SimpleBitVector8  v) = rankWords (toList v)
  {-# INLINABLE bitRank #-}

instance BitRank SimpleBitVector16 where
  bitRank (SimpleBitVector16 v) = rankWords (toList v)
  {-# INLINABLE bitRank #-}

instance BitRank SimpleBitVector32 where
  bitRank (SimpleBitVector32 v) = rankWords (toList v)
  {-# INLINABLE bitRank #-}

instance BitRank SimpleBitVector64 where
  bitRank (SimpleBitVector64 v) = rankWords (toList v)
  {-# INLINABLE bitRank #-}

instance BitSelect SimpleBitVector8 where
  bitSelect (SimpleBitVector8  v) = selectWords 0 (toList v)
  {-# INLINABLE bitSelect #-}

instance BitSelect SimpleBitVector16 where
  bitSelect (SimpleBitVector16 v) = selectWords 0 (toList v)
  {-# INLINABLE bitSelect #-}

instance BitSelect SimpleBitVector32 where
  bitSelect (SimpleBitVector32 v) = selectWords 0 (toList v)
  {-# INLINABLE bitSelect #-}

instance BitSelect SimpleBitVector64 where
  bitSelect (SimpleBitVector64 v) = selectWords 0 (toList v)
  {-# INLINABLE bitSelect #-}

rankWords :: (Num a, PopCount a, BitRank a, BitLength a) => [a] -> Int64 -> Int64
rankWords ws n = if remainder == 0
    then predRank
    else predRank + partRank
  where
    partRank = bitRank r remainder
    remainder = n `mod` bitLen
    (ls, rs) = P.splitAt (fromIntegral $ n `quot` bitLen) ws
    predRank = P.sum (P.map (fromIntegral . popCount) ls)
    r = headDef 0 rs
    bitLen = bitLength (P.head ws)
{-# INLINABLE rankWords #-}

selectWords :: (PopCount v, BitSelect v, BitLength v) => Int64 -> [v] -> Int64 -> Int64
selectWords _ [] r = r + 1
selectWords n (w:ws) r = if pc < n
    then selectWords (n - pc) ws (r + bitLength w)
    else bitSelect w n + r
  where
    pc = popCount w
{-# INLINABLE selectWords #-}
