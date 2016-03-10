{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0Spec (spec) where

import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitString
import           HaskellWorks.Data.Bits.PopCount.PopCount0
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

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

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.RankSelect.InternalSpec" $ do
  describe "For Word8" $ do
    it "select False 10010010 over [0..5] should be 023568" $ do
      let (Just bs) = fromBitString "10010010" :: Maybe Word8
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
  describe "For Word64" $ do
    it "rank0 and select0 for Word64 form a galois connection" $ property $
      \(Count_0_32 i) (w :: Word32) -> 1 <= i && i <= popCount0 w ==>
        rank0 w (select0 w i) == i && select0 w (rank0 w (fromIntegral i)) <= (fromIntegral i)
  describe "For (DVS.Vector Word8)" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let (Just bs) = fromBitString "10010010" :: Maybe (DVS.Vector Word8)
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
