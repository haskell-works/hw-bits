{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE NoImplicitPrelude          #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Succinct.Simple
    ( -- * Simple bit vector types
      SimpleBitVector64(..)
    ) where

-- import           Data.Bits
import           Data.Int
import           Data.Vector
import           Data.Word
import           HaskellWorks.Data.Succinct.Internal
import           Prelude as P
import           Safe

newtype SimpleBitVector8  = SimpleBitVector8  (Vector Word8 ) deriving (Eq, Show)
newtype SimpleBitVector16 = SimpleBitVector16 (Vector Word16) deriving (Eq, Show)
newtype SimpleBitVector32 = SimpleBitVector32 (Vector Word32) deriving (Eq, Show)
newtype SimpleBitVector64 = SimpleBitVector64 (Vector Word64) deriving (Eq, Show)

-- rankW8b :: Word8 -> Int64 -> Int64
-- rankW8b = undefined

rankW8s :: [Word8] -> Int64 -> Int64
rankW8s ws n = if remainder == 0
    then predRank
    else predRank + fromIntegral (popCount wordPart)
  where
    wordPart = ((fromIntegral r .<. remainder) .&. 0xff) .>. remainder :: Word8
    remainder = n `mod` 8
    (ls, rs) = P.splitAt (fromIntegral $ n `quot` 8) ws
    predRank = P.sum (P.map (fromIntegral . popCount) ls)
    r = headDef 0 rs

rankW16s :: [Word16] -> Int64 -> Int64
rankW16s ws n = if remainder == 0
    then predRank
    else predRank + fromIntegral (popCount wordPart)
  where
    wordPart = ((fromIntegral r .<. remainder) .&. 0xffff) .>. remainder :: Word16
    remainder = n `mod` 16
    (ls, rs) = P.splitAt (fromIntegral $ n `quot` 16) ws
    predRank = P.sum (P.map (fromIntegral . popCount) ls)
    r = headDef 0 rs

rankW32s :: [Word32] -> Int64 -> Int64
rankW32s ws n = if remainder == 0
    then predRank
    else predRank + fromIntegral (popCount wordPart)
  where
    wordPart = ((fromIntegral r .<. remainder) .&. 0xffffffff) .>. remainder :: Word32
    remainder = n `mod` 32
    (ls, rs) = P.splitAt (fromIntegral $ n `quot` 32) ws
    predRank = P.sum (P.map (fromIntegral . popCount) ls)
    r = headDef 0 rs

rankW64s :: [Word64] -> Int64 -> Int64
rankW64s ws n = if remainder == 0
    then predRank
    else predRank + fromIntegral (popCount wordPart)
  where
    wordPart = ((fromIntegral r .<. remainder) .&. 0xffffffffffffffff) .>. remainder :: Word64
    remainder = n `mod` 64
    (ls, rs) = P.splitAt (fromIntegral $ n `quot` 64) ws
    predRank = P.sum (P.map (fromIntegral . popCount) ls)
    r = headDef 0 rs

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
  bitRank n (SimpleBitVector8  v) = rankW8s (toList v) n

instance BitRank SimpleBitVector16 where
  bitRank n (SimpleBitVector16 v) = rankW16s (toList v) n

instance BitRank SimpleBitVector32 where
  bitRank n (SimpleBitVector32 v) = rankW32s (toList v) n

instance BitRank SimpleBitVector64 where
  bitRank n (SimpleBitVector64 v) = rankW64s (toList v) n

instance BitSelect SimpleBitVector64 where
  bitSelect = undefined
