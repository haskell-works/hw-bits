{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Positioning
  ( Count(..)
  , Position(..)
  , lastPositionOf
  , toCount
  , toPosition
  ) where

import           Data.Int
import           Data.Word
import           System.Random

-- | A value representing a count
newtype Count = Count { getCount :: Word64 }
  deriving (Eq, Num, Ord, Enum, Integral, Real, Random)

instance Show Count where
    show (Count w64) = show w64

    -- | A value representing a position
newtype Position = Position { getPosition :: Int64 }
  deriving (Eq, Num, Ord, Enum, Real, Integral)

instance Show Position where
    show (Position n) = show n

-- | Convert a count to a position
toPosition :: Count -> Position
toPosition (Count n) = Position (fromIntegral n)

-- | Convert a count to a count
toCount :: Position -> Count
toCount (Position n) = Count (fromIntegral n)

-- | Get largest position in a sequenced container of size count.
lastPositionOf :: Count -> Position
lastPositionOf (Count c)  = Position (fromIntegral c - 1)
