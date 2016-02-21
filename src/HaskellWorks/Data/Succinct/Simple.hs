{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE UndecidableInstances       #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Succinct.Simple
    ( -- * Simple bit vector types
      Simple(..)
    ) where

import           Data.Int
import           Data.Vector
import           Data.Word
import           HaskellWorks.Data.Succinct.Internal
import           Prelude                             as P
import           Safe

newtype Simple a = Simple a deriving (Eq, Show)

instance TestBit (Simple (Vector Word8)) where
  Simple v .?. n = case n `quotRem` bitLength (v ! 0) of
    (q, r) -> (v ! fromIntegral q) .?. fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit (Simple (Vector Word16)) where
  Simple v .?. n = case n `quotRem` bitLength (v ! 0) of
    (q, r) -> (v ! fromIntegral q) .?. fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit (Simple (Vector Word32)) where
  Simple v .?. n = case n `quotRem` bitLength (v ! 0) of
    (q, r) -> (v ! fromIntegral q) .?. fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit (Simple (Vector Word64)) where
  Simple v .?. n = case n `quotRem` bitLength (v ! 0) of
    (q, r) -> (v ! fromIntegral q) .?. fromIntegral r
  {-# INLINABLE (.?.) #-}

instance BitRank (Simple (Vector Word8)) where
  bitRank (Simple v) = rankWords (toList v)
  {-# INLINABLE bitRank #-}

instance BitRank (Simple (Vector Word16)) where
  bitRank (Simple v) = rankWords (toList v)
  {-# INLINABLE bitRank #-}

instance BitRank (Simple (Vector Word32)) where
  bitRank (Simple v) = rankWords (toList v)
  {-# INLINABLE bitRank #-}

instance BitRank (Simple (Vector Word64)) where
  bitRank (Simple v) = rankWords (toList v)
  {-# INLINABLE bitRank #-}

instance BitSelect (Simple (Vector Word8)) where
  bitSelect (Simple v) = selectWords 0 (toList v)
  {-# INLINABLE bitSelect #-}

instance BitSelect (Simple (Vector Word16))  where
  bitSelect (Simple v) = selectWords 0 (toList v)
  {-# INLINABLE bitSelect #-}

instance BitSelect (Simple (Vector Word32))  where
  bitSelect (Simple v) = selectWords 0 (toList v)
  {-# INLINABLE bitSelect #-}

instance BitSelect (Simple (Vector Word64))  where
  bitSelect (Simple v) = selectWords 0 (toList v)
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
