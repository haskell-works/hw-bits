{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.Types.Builtin where

import Data.Vector.Storable
import HaskellWorks.Data.Bits.BitWise

-- | Type wrapper to prefer builting operations
newtype Builtin a = Builtin a deriving (BitWise, Eq, Show, Storable)
