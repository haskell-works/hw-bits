module HaskellWorks.Data.Arbitrary.Count where

import           Data.Word
import           HaskellWorks.Data.Positioning
import           Test.QuickCheck

newtype Count_0_8  = Count_0_8  Count deriving (Eq, Show)
newtype Count_0_16 = Count_0_16 Count deriving (Eq, Show)
newtype Count_0_32 = Count_0_32 Count deriving (Eq, Show)
newtype Count_0_64 = Count_0_64 Count deriving (Eq, Show)

instance Arbitrary Count_0_8 where
  arbitrary = do
     n <- choose (0, 8 :: Word64)
     return (Count_0_8 (Count n))

instance Arbitrary Count_0_16 where
 arbitrary = do
    n <- choose (0, 16 :: Word64)
    return (Count_0_16 (Count n))

instance Arbitrary Count_0_32 where
 arbitrary = do
    n <- choose (0, 32 :: Word64)
    return (Count_0_32 (Count n))

instance Arbitrary Count_0_64 where
 arbitrary = do
    n <- choose (0, 64 :: Word64)
    return (Count_0_64 (Count n))
