{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.RankSelect.InternalSpec (spec) where

import qualified Data.Vector.Storable                           as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitString
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.PopCount.PopCount1
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Internal
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
  describe "For [Bool]" $ do
    it "rank True 10010010 over [0..8] should be 011122233" $
      let (Just bs) = fromBitString "10010010" :: Maybe [Bool] in
      fmap (rank True bs) [0..8] `shouldBe` [0, 1, 1, 1, 2, 2, 2, 3, 3]
    it "rank True 10010010 over [0..8] should be 001223445" $
      let (Just bs) = fromBitString "10010010" :: Maybe [Bool] in
      fmap (rank False bs) [0..8] `shouldBe` [0, 0, 1, 2, 2, 3, 4, 4, 5]
    it "select True 10010010 over [0..3] should be 0147" $
      let (Just bs) = fromBitString "10010010" :: Maybe [Bool] in
      fmap (select True bs) [0..3] `shouldBe` [0, 1, 4, 7]
    it "select False 10010010 over [0..5] should be 023568" $
      let (Just bs) = fromBitString "10010010" :: Maybe [Bool] in
      fmap (select False bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
    it "Rank and select form a galois connection" $
      property $ \(bs :: [Bool]) ->
      forAll (choose (0, popCount1 bs)) $ \(c :: Count) ->
        rank True bs (select True bs c) == c
