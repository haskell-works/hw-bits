{-# LANGUAGE MultiParamTypeClasses #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.PopCount
    ( -- * Bit map
      PopCount(..)
    , module X
    ) where

import           HaskellWorks.Data.Bits.PopCount.PopCount0 as X
import           HaskellWorks.Data.Bits.PopCount.PopCount1 as X
import           HaskellWorks.Data.Positioning

class PopCount v e where
  popCount :: e -> v -> Count
