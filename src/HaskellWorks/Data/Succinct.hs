-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Succinct
    ( -- * Rank & Select
      BitRank(..)
    , BitSelect(..)
    , BitWise(..)
    , PopCount(..)
    , Rank(..)
    , Select(..)
    , Shift(..)
    , Simple(..)
    , TestBit(..)
    ) where

import           HaskellWorks.Data.Succinct.Internal
import           HaskellWorks.Data.Succinct.Simple
