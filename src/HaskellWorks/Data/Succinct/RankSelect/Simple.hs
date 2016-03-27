{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NoImplicitPrelude          #-}
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

import qualified Data.Vector                                                as DV
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Vector.VectorLike
import qualified Prelude                                                    as P

newtype Simple a = Simple a deriving (P.Eq, BitShow)

instance P.Functor Simple where
  fmap f (Simple a) = Simple (f a)

getSimple :: Simple a -> a
getSimple (Simple a) = a

instance BitShow a => P.Show (Simple a) where
  show (Simple bs) = bitShow bs

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
  rank1 (Simple v) = rank1 v
  {-# INLINABLE rank1 #-}

instance Rank1 (Simple (DV.Vector Word16)) where
  rank1 (Simple v) = rank1 v
  {-# INLINABLE rank1 #-}

instance Rank1 (Simple (DV.Vector Word32)) where
  rank1 (Simple v) = rank1 v
  {-# INLINABLE rank1 #-}

instance Rank1 (Simple (DV.Vector Word64)) where
  rank1 (Simple v) = rank1 v
  {-# INLINABLE rank1 #-}

instance Select1 (Simple (DV.Vector Word8)) where
  select1 (Simple v) = select1 v
  {-# INLINABLE select1 #-}

instance Select1 (Simple (DV.Vector Word16))  where
  select1 (Simple v) = select1 v
  {-# INLINABLE select1 #-}

instance Select1 (Simple (DV.Vector Word32))  where
  select1 (Simple v) = select1 v
  {-# INLINABLE select1 #-}

instance Select1 (Simple (DV.Vector Word64))  where
  select1 (Simple v) = select1 v
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
  rank1 (Simple v) = rank1 v
  {-# INLINABLE rank1 #-}

instance Rank1 (Simple (DVS.Vector Word16)) where
  rank1 (Simple v) = rank1 v
  {-# INLINABLE rank1 #-}

instance Rank1 (Simple (DVS.Vector Word32)) where
  rank1 (Simple v) = rank1 v
  {-# INLINABLE rank1 #-}

instance Rank1 (Simple (DVS.Vector Word64)) where
  rank1 (Simple v) = rank1 v
  {-# INLINABLE rank1 #-}

instance Select1 (Simple (DVS.Vector Word8)) where
  select1 (Simple v) = select1 v
  {-# INLINABLE select1 #-}

instance Select1 (Simple (DVS.Vector Word16))  where
  select1 (Simple v) = select1 v
  {-# INLINABLE select1 #-}

instance Select1 (Simple (DVS.Vector Word32))  where
  select1 (Simple v) = select1 v
  {-# INLINABLE select1 #-}

instance Select1 (Simple (DVS.Vector Word64))  where
  select1 (Simple v) = select1 v
  {-# INLINABLE select1 #-}
