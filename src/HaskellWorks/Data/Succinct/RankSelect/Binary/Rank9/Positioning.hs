{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.Rank9.Positioning where

import           Data.Word
import           Test.QuickCheck as QuickCheck

newtype Count = Count { getCount :: Word64 }
  deriving (Eq, Num, Ord, Enum, Real, Integral)

instance Show Count where
    show (Count w64) = show w64

instance Arbitrary Count where
    arbitrary = fmap Count arbitrary

newtype Position = Position { getPosition :: Int }
  deriving (Eq, Num, Ord, Enum, Real, Integral)

instance Show Position where
    show (Position n) = show n

arbitraryNonNegative :: QuickCheck.Gen Int
arbitraryNonNegative = fmap QuickCheck.getNonNegative arbitrary

instance Arbitrary Position where
    arbitrary = fmap Position arbitraryNonNegative
