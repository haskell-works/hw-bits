{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1Spec (spec) where

import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Arbitrary.Count
import           HaskellWorks.Data.Bits.BitString
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.PopCount.PopCount1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.RankSelect.InternalSpec" $ do
  describe "For [Bool]" $ do
    it "select False 110000011000000001000000 over [0..5] should be 023568" $
      let (Just bs) = fromBitString "110000011000000001000000" :: Maybe [Bool] in
      fmap (select1 bs) [0..5] `shouldBe` [0, 1, 2, 8, 9, 18]
    it "select1 1101101000 over [0..5]" $
      let (Just bs) = fromBitString "1101101000" :: Maybe [Bool] in
      fmap (select1 bs) [0..5] `shouldBe` [0, 1, 2, 4, 5, 7]
  describe "For Word8" $ do
    it "select True 10010010 over [0..3] should be 0147" $ do
      let (Just bs) = fromBitString "10010010" :: Maybe Word8
      fmap (select1 bs) [0..3] `shouldBe` [0, 1, 4, 7]
  describe "For Word64" $ do
    it "rank1 for Word16 and Word64 should give same answer for bits 0-7" $ property $
      \(Count_0_8  i) (w :: Word8 ) -> rank1 w i == rank1 (fromIntegral w :: Word64) i
    it "rank1 for Word16 and Word64 should give same answer for bits 0-15" $ property $
      \(Count_0_16 i) (w :: Word16) -> rank1 w i == rank1 (fromIntegral w :: Word64) i
    it "rank1 for Word32 and Word64 should give same answer for bits 0-31" $ property $
      \(Count_0_32 i) (w :: Word32) -> rank1 w i == rank1 (fromIntegral w :: Word64) i
    it "rank1 for Word32 and Word64 should give same answer for bits 32-64" $ property $
      \(Count_0_32 i) (v :: Word32) (w :: Word32) ->
        let v64 = fromIntegral v :: Word64 in
        let w64 = fromIntegral w :: Word64 in
        rank1 v i + popCount1 w == rank1 ((v64 .<. 32) .|. w64) (i + 32)
    it "rank1 and select1 for Word64 form a galois connection" $ property $
      \(Count_0_32 i) (w :: Word32) -> 1 <= i && i <= popCount1 w ==>
        rank1 w (select1 w i) == i && select1 w (rank1 w (fromIntegral i)) <= (fromIntegral i)
  describe "For (DVS.Vector Word8)" $ do
    it "select1 11000001 10000000 01000000 over [0..5] should be 023568" $ do
      let (Just bs) = fromBitString "11000001 10000000 01000000" :: Maybe (DVS.Vector Word8)
      fmap (select1 bs) [0..5] `shouldBe` [0, 1, 2, 8, 9, 18]
    it "select1 1101101000 over [0..5]" $ do
      let (Just bs) = fromBitString "1101101000" :: Maybe (DVS.Vector Word8)
      fmap (select1 bs) [0..5] `shouldBe` [0, 1, 2, 4, 5, 7]
    it "select1 11011010 00000000 over [0..5]" $ do
      let (Just bs) = fromBitString "11011010 00000000" :: Maybe (DVS.Vector Word8)
      fmap (select1 bs) [0..5] `shouldBe` [0, 1, 2, 4, 5, 7]
