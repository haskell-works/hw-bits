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

newtype Count = Count { getCount :: Word64 }
  deriving (Eq, Num, Ord, Enum, Integral, Real, Random)

instance Show Count where
    show (Count w64) = show w64

newtype Position = Position { getPosition :: Int64 }
  deriving (Eq, Num, Ord, Enum, Real, Integral)

instance Show Position where
    show (Position n) = show n

toPosition :: Count -> Position
toPosition (Count n) = Position (fromIntegral n)

toCount :: Position -> Count
toCount (Position n) = Count (fromIntegral n)

lastPositionOf :: Count -> Position
lastPositionOf (Count c)  = Position (fromIntegral c - 1)
