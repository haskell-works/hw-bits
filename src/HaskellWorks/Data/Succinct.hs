{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Succinct
    ( -- * Sink
      Rank(..)
    , Select(..)
    ) where

import Data.Int

class Rank v a where
  rank :: Eq a => Int64 -> a -> v -> Bool

class Select v where
  select :: Eq a => Int64 -> v -> a -> Bool
