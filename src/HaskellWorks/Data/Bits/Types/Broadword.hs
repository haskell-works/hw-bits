{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.Types.Broadword where

import Data.Vector.Storable
import HaskellWorks.Data.Bits.BitWise

newtype Broadword a = Broadword a deriving (BitWise, Eq, Show, Storable)
