{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Succinct.Positioning where

import Data.Int
import Data.Word

newtype Count = Count { getCount :: Word64 }
  deriving (Eq, Num, Ord, Enum, Real, Integral, Show)

newtype Position = Position { getPosition :: Int64 }
  deriving (Eq, Num, Ord, Enum, Real, Integral, Show)
