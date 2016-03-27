{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0Spec (spec) where

import           Data.Maybe
import qualified Data.Vector                                                as DV
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Arbitrary.Count
import           HaskellWorks.Data.Bits.BitRead
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.PopCount.PopCount0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.RankSelect.InternalSpec" $ do
  describe "For [Bool]" $ do
    it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $
      let bs = fromJust $ bitRead "11000001 10000000 01000000" :: [Bool] in
      fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]
    it "select1 1101101000 over [0..5]" $
      let bs = fromJust $ bitRead "1101101 000" :: [Bool] in
      fmap (select0 bs) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
  describe "For Word8" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "10010010" :: Word8
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
  describe "For Word32" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "10010010" :: Word32
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
    it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "11000001 10000000 01000000" :: Word32
      fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]
    it "select0 1101101000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00" :: Word32
      fmap (select1 (comp bs)) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
    it "select0 11011010 00000000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: Word32
      fmap (select0 bs) [0..11] `shouldBe` [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  describe "For Word64" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "10010010" :: Word64
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
    it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "11000001 10000000 01000000" :: Word64
      fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]
    it "select0 1101101000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00" :: Word64
      fmap (select0 bs) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
    it "select0 11011010 00000000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: Word64
      fmap (select0 bs) [0..11] `shouldBe` [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  describe "For Word8-Word64" $ do
    it "rank0 for Word16 and Word64 should give same answer for bits 0-7" $ property $
      \(Count_0_8  i) (w :: Word8 ) -> rank0 w i == rank0 (fromIntegral w :: Word64) i
    it "rank0 for Word16 and Word64 should give same answer for bits 0-15" $ property $
      \(Count_0_16 i) (w :: Word16) -> rank0 w i == rank0 (fromIntegral w :: Word64) i
    it "rank0 for Word32 and Word64 should give same answer for bits 0-31" $ property $
      \(Count_0_32 i) (w :: Word32) -> rank0 w i == rank0 (fromIntegral w :: Word64) i
    it "rank0 for Word32 and Word64 should give same answer for bits 32-64" $ property $
      \(Count_0_32 i) (v :: Word32) (w :: Word32) ->
        let v64 = fromIntegral v :: Word64 in
        let w64 = fromIntegral w :: Word64 in
        rank0 v i + popCount0 w == rank0 ((v64 .<. 32) .|. w64) (i + 32)
    it "rank0 and select0 for Word64 form a galois connection" $ property $
      \(Count_0_32 i) (w :: Word32) -> 1 <= i && i <= popCount0 w ==>
        rank0 w (select0 w i) == i && select0 w (rank0 w (fromIntegral i)) <= fromIntegral i
  describe "For [Word8]" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "10010010" :: [Word8]
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
    it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "11000001 10000000 01000000" :: [Word8]
      fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]
    it "select0 1101101000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00" :: [Word8]
      fmap (select0 bs) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
    it "select0 11011010 00000000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: [Word8]
      fmap (select0 bs) [0..11] `shouldBe` [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  describe "For [Word16]" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "10010010" :: [Word16]
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
    it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "11000001 10000000 01000000" :: [Word16]
      fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]
    it "select0 1101101000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00" :: [Word16]
      fmap (select0 bs) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
    it "select0 11011010 00000000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: [Word16]
      fmap (select0 bs) [0..11] `shouldBe` [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  describe "For [Word32]" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "10010010" :: [Word32]
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
    it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "11000001 10000000 01000000" :: [Word32]
      fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]
    it "select0 1101101000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00" :: [Word32]
      fmap (select0 bs) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
    it "select0 11011010 00000000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: [Word32]
      fmap (select0 bs) [0..11] `shouldBe` [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  describe "For [Word64]" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "10010010" :: [Word64]
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
    it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "11000001 10000000 01000000" :: [Word64]
      fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]
    it "select0 1101101000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00" :: [Word64]
      fmap (select0 bs) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
    it "select0 11011010 00000000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: [Word64]
      fmap (select0 bs) [0..11] `shouldBe` [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  describe "For (DV.Vector Word8)" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "10010010" :: DV.Vector Word8
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
    it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "11000001 10000000 01000000" :: (DV.Vector Word8)
      fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]
    it "select0 1101101000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00" :: (DV.Vector Word8)
      fmap (select0 bs) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
    it "select0 11011010 00000000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: (DV.Vector Word8)
      fmap (select0 bs) [0..11] `shouldBe` [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  describe "For (DV.Vector Word16)" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "10010010" :: DV.Vector Word16
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
    it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "11000001 10000000 01000000" :: (DV.Vector Word16)
      fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]
    it "select0 1101101000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00" :: (DV.Vector Word16)
      fmap (select0 bs) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
    it "select0 11011010 00000000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: (DV.Vector Word16)
      fmap (select0 bs) [0..11] `shouldBe` [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  describe "For (DV.Vector Word32)" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "10010010" :: DV.Vector Word32
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
    it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "11000001 10000000 01000000" :: (DV.Vector Word32)
      fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]
    it "select0 1101101000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00" :: (DV.Vector Word32)
      fmap (select0 bs) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
    it "select0 11011010 00000000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: (DV.Vector Word32)
      fmap (select0 bs) [0..11] `shouldBe` [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  describe "For (DV.Vector Word64)" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "10010010" :: DV.Vector Word64
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
    it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "11000001 10000000 01000000" :: (DV.Vector Word64)
      fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]
    it "select0 1101101000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00" :: (DV.Vector Word64)
      fmap (select0 bs) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
    it "select0 11011010 00000000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: (DV.Vector Word64)
      fmap (select0 bs) [0..11] `shouldBe` [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  describe "For (DVS.Vector Word8)" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "10010010" :: DVS.Vector Word8
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
    it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "11000001 10000000 01000000" :: (DVS.Vector Word8)
      fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]
    it "select0 1101101000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00" :: (DVS.Vector Word8)
      fmap (select0 bs) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
    it "select0 11011010 00000000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: (DVS.Vector Word8)
      fmap (select0 bs) [0..11] `shouldBe` [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  describe "For (DVS.Vector Word16)" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "10010010" :: DVS.Vector Word16
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
    it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "11000001 10000000 01000000" :: (DVS.Vector Word16)
      fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]
    it "select0 1101101000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00" :: (DVS.Vector Word16)
      fmap (select0 bs) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
    it "select0 11011010 00000000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: (DVS.Vector Word16)
      fmap (select0 bs) [0..11] `shouldBe` [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  describe "For (DVS.Vector Word32)" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "10010010" :: DVS.Vector Word32
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
    it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "11000001 10000000 01000000" :: (DVS.Vector Word32)
      fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]
    it "select0 1101101000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00" :: (DVS.Vector Word32)
      fmap (select0 bs) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
    it "select0 11011010 00000000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: (DVS.Vector Word32)
      fmap (select0 bs) [0..11] `shouldBe` [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]
  describe "For (DVS.Vector Word64)" $ do
    it "select0 10010010 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "10010010" :: DVS.Vector Word64
      fmap (select0 bs) [0..5] `shouldBe` [0, 2, 3, 5, 6, 8]
    it "select0 11000001 10000000 01000000 over [0..5] should be 023568" $ do
      let bs = fromJust $ bitRead "11000001 10000000 01000000" :: (DVS.Vector Word64)
      fmap (select0 bs) [0..19] `shouldBe` [0, 3, 4, 5, 6, 7, 10, 11, 12, 13, 14, 15, 16, 17, 19, 20, 21, 22, 23, 24]
    it "select0 1101101000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00" :: (DVS.Vector Word64)
      fmap (select0 bs) [0..5] `shouldBe` [0, 3, 6, 8, 9, 10]
    it "select0 11011010 00000000 over [0..5]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: (DVS.Vector Word64)
      fmap (select0 bs) [0..11] `shouldBe` [0, 3, 6, 8, 9, 10, 11, 12, 13, 14, 15, 16]
