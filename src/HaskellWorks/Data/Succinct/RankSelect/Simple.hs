{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
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

import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1

newtype Simple a = Simple a deriving (Eq, BitShow)

deriving instance Functor Simple

getSimple :: Simple a -> a
getSimple (Simple a) = a

instance BitShow a => Show (Simple a) where
  show (Simple bs) = bitShow bs

deriving instance Rank0   a => Rank0   (Simple a)
deriving instance Rank1   a => Rank1   (Simple a)
deriving instance Select0 a => Select0 (Simple a)
deriving instance Select1 a => Select1 (Simple a)
deriving instance TestBit a => TestBit (Simple a)
