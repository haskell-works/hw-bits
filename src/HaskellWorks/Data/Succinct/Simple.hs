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
      Simple(..)
    ) where

import           Data.Int
import qualified Data.Vector                         as DV
import qualified Data.Vector.Storable                as DVS
import           Data.Word
import           HaskellWorks.Data.MonoList
import           HaskellWorks.Data.Succinct.Internal
import qualified Prelude                             as P
import           Safe

newtype Simple a = Simple a deriving (P.Eq, P.Show)

instance TestBit (Simple (DV.Vector Word8)) where
  Simple v .?. n = case n `P.quotRem` bitLength (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit (Simple (DV.Vector Word16)) where
  Simple v .?. n = case n `P.quotRem` bitLength (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit (Simple (DV.Vector Word32)) where
  Simple v .?. n = case n `P.quotRem` bitLength (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit (Simple (DV.Vector Word64)) where
  Simple v .?. n = case n `P.quotRem` bitLength (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance BitRank (Simple (DV.Vector Word8)) where
  bitRank (Simple v) = rankWords (monoList v)
  {-# INLINABLE bitRank #-}

instance BitRank (Simple (DV.Vector Word16)) where
  bitRank (Simple v) = rankWords (monoList v)
  {-# INLINABLE bitRank #-}

instance BitRank (Simple (DV.Vector Word32)) where
  bitRank (Simple v) = rankWords (monoList v)
  {-# INLINABLE bitRank #-}

instance BitRank (Simple (DV.Vector Word64)) where
  bitRank (Simple v) = rankWords (monoList v)
  {-# INLINABLE bitRank #-}

instance BitSelect (Simple (DV.Vector Word8)) where
  bitSelect (Simple v) = selectWords 0 (monoList v)
  {-# INLINABLE bitSelect #-}

instance BitSelect (Simple (DV.Vector Word16))  where
  bitSelect (Simple v) = selectWords 0 (monoList v)
  {-# INLINABLE bitSelect #-}

instance BitSelect (Simple (DV.Vector Word32))  where
  bitSelect (Simple v) = selectWords 0 (monoList v)
  {-# INLINABLE bitSelect #-}

instance BitSelect (Simple (DV.Vector Word64))  where
  bitSelect (Simple v) = selectWords 0 (monoList v)
  {-# INLINABLE bitSelect #-}

instance TestBit (Simple (DVS.Vector Word8)) where
  Simple v .?. n = case n `P.quotRem` bitLength (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit (Simple (DVS.Vector Word16)) where
  Simple v .?. n = case n `P.quotRem` bitLength (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit (Simple (DVS.Vector Word32)) where
  Simple v .?. n = case n `P.quotRem` bitLength (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit (Simple (DVS.Vector Word64)) where
  Simple v .?. n = case n `P.quotRem` bitLength (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance BitRank (Simple (DVS.Vector Word8)) where
  bitRank (Simple v) = rankWords (monoList v)
  {-# INLINABLE bitRank #-}

instance BitRank (Simple (DVS.Vector Word16)) where
  bitRank (Simple v) = rankWords (monoList v)
  {-# INLINABLE bitRank #-}

instance BitRank (Simple (DVS.Vector Word32)) where
  bitRank (Simple v) = rankWords (monoList v)
  {-# INLINABLE bitRank #-}

instance BitRank (Simple (DVS.Vector Word64)) where
  bitRank (Simple v) = rankWords (monoList v)
  {-# INLINABLE bitRank #-}

instance BitSelect (Simple (DVS.Vector Word8)) where
  bitSelect (Simple v) = selectWords 0 (monoList v)
  {-# INLINABLE bitSelect #-}

instance BitSelect (Simple (DVS.Vector Word16))  where
  bitSelect (Simple v) = selectWords 0 (monoList v)
  {-# INLINABLE bitSelect #-}

instance BitSelect (Simple (DVS.Vector Word32))  where
  bitSelect (Simple v) = selectWords 0 (monoList v)
  {-# INLINABLE bitSelect #-}

instance BitSelect (Simple (DVS.Vector Word64))  where
  bitSelect (Simple v) = selectWords 0 (monoList v)
  {-# INLINABLE bitSelect #-}

rankWords :: (P.Num a, PopCount a, BitRank a, BitLength a) => [a] -> Int64 -> Int64
rankWords ws n = if remainder P.== 0
    then predRank
    else predRank P.+ partRank
  where
    partRank = bitRank r remainder
    remainder = n `P.mod` bitLen
    (ls, rs) = P.splitAt (P.fromIntegral P.$ n `P.quot` bitLen) ws
    predRank = P.sum (P.map (P.fromIntegral P.. popCount) ls)
    r = headDef 0 rs
    bitLen = bitLength (P.head ws)
{-# INLINABLE rankWords #-}

selectWords :: (PopCount v, BitSelect v, BitLength v) => Int64 -> [v] -> Int64 -> Int64
selectWords _ [] r = r P.+ 1
selectWords n (w:ws) r = if pc P.< n
    then selectWords (n P.- pc) ws (r P.+ bitLength w)
    else bitSelect w n P.+ r
  where
    pc = popCount w
{-# INLINABLE selectWords #-}
