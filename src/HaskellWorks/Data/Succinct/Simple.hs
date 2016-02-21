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

instance TestBit SimpleBitVector16 where
  SimpleBitVector16 v .?. n = case n `quotRem` bitLength (v ! 0) of
    (q, r) -> (v ! fromIntegral q) .?. fromIntegral r

instance TestBit SimpleBitVector32 where
  SimpleBitVector32 v .?. n = case n `quotRem` bitLength (v ! 0) of
    (q, r) -> (v ! fromIntegral q) .?. fromIntegral r

instance TestBit SimpleBitVector64 where
  SimpleBitVector64 v .?. n = case n `quotRem` bitLength (v ! 0) of
    (q, r) -> (v ! fromIntegral q) .?. fromIntegral r

instance BitRank SimpleBitVector8 where
  bitRank (SimpleBitVector8  v) = rankW8s (toList v)

instance BitRank SimpleBitVector16 where
  bitRank (SimpleBitVector16 v) = rankW16s (toList v)

instance BitRank SimpleBitVector32 where
  bitRank (SimpleBitVector32 v) = rankW32s (toList v)

instance BitRank SimpleBitVector64 where
  bitRank (SimpleBitVector64 v) = rankW64s (toList v)

instance BitSelect SimpleBitVector8 where
  bitSelect (SimpleBitVector8  v) = selectW8s 0 (toList v)

instance BitSelect SimpleBitVector16 where
  bitSelect (SimpleBitVector16 v) = selectW16s 0 (toList v)

instance BitSelect SimpleBitVector32 where
  bitSelect (SimpleBitVector32 v) = selectW32s 0 (toList v)

instance BitSelect SimpleBitVector64 where
  bitSelect (SimpleBitVector64 v) = selectW64s 0 (toList v)

rankW8s :: [Word8] -> Int64 -> Int64
rankW8s ws n = if remainder == 0
    then predRank
    else predRank + partRank
  where
    partRank = bitRank r remainder
    remainder = n `mod` 8
    (ls, rs) = P.splitAt (fromIntegral $ n `quot` 8) ws
    predRank = P.sum (P.map (fromIntegral . popCount) ls)
    r = headDef 0 rs

rankW16s :: [Word16] -> Int64 -> Int64
rankW16s ws n = if remainder == 0
    then predRank
    else predRank + partRank
  where
    partRank = bitRank r remainder
    remainder = n `mod` 16
    (ls, rs) = P.splitAt (fromIntegral $ n `quot` 16) ws
    predRank = P.sum (P.map (fromIntegral . popCount) ls)
    r = headDef 0 rs

rankW32s :: [Word32] -> Int64 -> Int64
rankW32s ws n = if remainder == 0
    then predRank
    else predRank + partRank
  where
    partRank = bitRank r remainder
    remainder = n `mod` 32
    (ls, rs) = P.splitAt (fromIntegral $ n `quot` 32) ws
    predRank = P.sum (P.map (fromIntegral . popCount) ls)
    r = headDef 0 rs

rankW64s :: [Word64] -> Int64 -> Int64
rankW64s ws n = if remainder == 0
    then predRank
    else predRank + partRank
  where
    partRank = bitRank r remainder
    remainder = n `mod` 64
    (ls, rs) = P.splitAt (fromIntegral $ n `quot` 64) ws
    predRank = P.sum (P.map (fromIntegral . popCount) ls)
    r = headDef 0 rs

selectW8s :: Int64 -> [Word8] -> Int64 -> Int64
selectW8s _ [] r = r + 1
selectW8s n (w:ws) r = if pc < n
    then selectW8s (n - pc) ws (r + bitLength w)
    else bitSelect w n + r
  where
    pc = popCount w

selectW16s :: Int64 -> [Word16] -> Int64 -> Int64
selectW16s _ [] r = r + 1
selectW16s n (w:ws) r = if pc < n
    then selectW16s (n - pc) ws (r + bitLength w)
    else bitSelect w n + r
  where
    pc = popCount w

selectW32s :: Int64 -> [Word32] -> Int64 -> Int64
selectW32s _ [] r = r + 1
selectW32s n (w:ws) r = if pc < n
    then selectW32s (n - pc) ws (r + bitLength w)
    else bitSelect w n + r
  where
    pc = popCount w

selectW64s :: Int64 -> [Word64] -> Int64 -> Int64
selectW64s _ [] r = r + 1
selectW64s n (w:ws) r = if pc < n
    then selectW64s (n - pc) ws (r + bitLength w)
    else bitSelect w n + r
  where
    pc = popCount w
