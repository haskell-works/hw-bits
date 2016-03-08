{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverlappingInstances       #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeSynonymInstances       #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Succinct.RankSelect.Simple
    ( -- * Simple bit vector types
      Simple(..)
    , getSimple
    ) where

import qualified Data.Vector                                    as DV
import qualified Data.Vector.Storable                           as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitString
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Internal
import           HaskellWorks.Data.VectorLike
import qualified Prelude                                        as P
import           Safe

newtype Simple a = Simple a deriving (P.Eq)

getSimple :: Simple a -> a
getSimple (Simple a) = a

instance forall a. ToBitString a => P.Show (Simple a) where
  show (Simple bs) = toBitString bs

instance TestBit (Simple (DV.Vector Word8)) where
  Simple v .?. n = case n `P.quotRem` endPosition (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit (Simple (DV.Vector Word16)) where
  Simple v .?. n = case n `P.quotRem` endPosition (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit (Simple (DV.Vector Word32)) where
  Simple v .?. n = case n `P.quotRem` endPosition (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit (Simple (DV.Vector Word64)) where
  Simple v .?. n = case n `P.quotRem` endPosition (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance Rank0 (Simple [P.Bool]) where
  rank0 (Simple v) = rank0 v
  {-# INLINABLE rank0 #-}

instance Select0 (Simple [P.Bool]) where
  select0 (Simple v) = select0 v
  {-# INLINABLE select0 #-}

instance Rank1 (Simple [P.Bool]) where
  rank1 (Simple v) = rank1 v
  {-# INLINABLE rank1 #-}

instance Select1 (Simple [P.Bool]) where
  select1 (Simple v) = select1 v
  {-# INLINABLE select1 #-}

instance Rank1 (Simple (DV.Vector Word8)) where
  rank1 (Simple v) = rankWords (toList v)
  {-# INLINABLE rank1 #-}

instance Rank1 (Simple (DV.Vector Word16)) where
  rank1 (Simple v) = rankWords (toList v)
  {-# INLINABLE rank1 #-}

instance Rank1 (Simple (DV.Vector Word32)) where
  rank1 (Simple v) = rankWords (toList v)
  {-# INLINABLE rank1 #-}

instance Rank1 (Simple (DV.Vector Word64)) where
  rank1 (Simple v) = rankWords (toList v)
  {-# INLINABLE rank1 #-}

instance Select1 (Simple (DV.Vector Word8)) where
  select1 (Simple v) = selectWords 0 (toList v)
  {-# INLINABLE select1 #-}

instance Select1 (Simple (DV.Vector Word16))  where
  select1 (Simple v) = selectWords 0 (toList v)
  {-# INLINABLE select1 #-}

instance Select1 (Simple (DV.Vector Word32))  where
  select1 (Simple v) = selectWords 0 (toList v)
  {-# INLINABLE select1 #-}

instance Select1 (Simple (DV.Vector Word64))  where
  select1 (Simple v) = selectWords 0 (toList v)
  {-# INLINABLE select1 #-}

instance TestBit (Simple (DVS.Vector Word8)) where
  Simple v .?. n = case n `P.quotRem` endPosition (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit (Simple (DVS.Vector Word16)) where
  Simple v .?. n = case n `P.quotRem` endPosition (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit (Simple (DVS.Vector Word32)) where
  Simple v .?. n = case n `P.quotRem` endPosition (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance TestBit (Simple (DVS.Vector Word64)) where
  Simple v .?. n = case n `P.quotRem` endPosition (v !!! 0) of
    (q, r) -> (v !!! P.fromIntegral q) .?. P.fromIntegral r
  {-# INLINABLE (.?.) #-}

instance Rank1 (Simple (DVS.Vector Word8)) where
  rank1 (Simple v) = rankWords (toList v)
  {-# INLINABLE rank1 #-}

instance Rank1 (Simple (DVS.Vector Word16)) where
  rank1 (Simple v) = rankWords (toList v)
  {-# INLINABLE rank1 #-}

instance Rank1 (Simple (DVS.Vector Word32)) where
  rank1 (Simple v) = rankWords (toList v)
  {-# INLINABLE rank1 #-}

instance Rank1 (Simple (DVS.Vector Word64)) where
  rank1 (Simple v) = rankWords (toList v)
  {-# INLINABLE rank1 #-}

instance Select1 (Simple (DVS.Vector Word8)) where
  select1 (Simple v) = selectWords 0 (toList v)
  {-# INLINABLE select1 #-}

instance Select1 (Simple (DVS.Vector Word16))  where
  select1 (Simple v) = selectWords 0 (toList v)
  {-# INLINABLE select1 #-}

instance Select1 (Simple (DVS.Vector Word32))  where
  select1 (Simple v) = selectWords 0 (toList v)
  {-# INLINABLE select1 #-}

instance Select1 (Simple (DVS.Vector Word64))  where
  select1 (Simple v) = selectWords 0 (toList v)
  {-# INLINABLE select1 #-}

rankWords :: (P.Num a, PopCount1 a, Rank1 a, BitLength a) => [a] -> Count -> Count
rankWords ws n = if remainder P.== 0
    then predRank
    else predRank P.+ partRank
  where
    partRank = rank1 r remainder
    remainder = n `P.mod` endPos
    (ls, rs) = P.splitAt (P.fromIntegral P.$ n `P.quot` endPos) ws
    predRank = P.sum (P.map (P.fromIntegral P.. popCount1) ls)
    r = headDef 0 rs
    endPos = bitLength (P.head ws)
{-# INLINABLE rankWords #-}

selectWords :: (PopCount1 v, Select1 v, BitLength v) => Count -> [v] -> Count -> Count
selectWords _ [] r = r P.+ 1
selectWords n (w:ws) r = if pc P.< n
    then selectWords (n P.- pc) ws (r P.+ bitLength w)
    else select1 w n P.+ r
  where
    pc = popCount1 w
{-# INLINABLE selectWords #-}
