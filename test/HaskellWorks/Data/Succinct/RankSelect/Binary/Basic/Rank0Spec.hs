{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0Spec
  ( genSpec
  , spec
  ) where

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
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

genSpec :: forall s. (BitRead s, Rank0 s) => s -> Spec
genSpec _ = describe "For Word8" $ do
  it "rank0 10010010 over [0..8] should be 001223445" $ do
    let bs = fromJust (bitRead "10010010") :: s
    fmap (rank0 bs) [0..8] `shouldBe` [0, 0, 1, 2, 2, 3, 4, 4, 5]

spec :: Spec
spec = describe "HaskellWorks.Data.Succinct.RankSelect.InternalSpec" $ do
  genSpec (undefined :: Word8)
  describe "For Word64" $ do
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
    it "rank0 and select1 for Word64 form a galois connection" $ property $
      \(Count_0_32 i) (w :: Word32) -> 1 <= i && i <= popCount0 w ==>
        rank0 w (select0 w i) == i && select0 w (rank0 w (fromIntegral i)) <= fromIntegral i
  describe "For [Word8]" $ do
    it "rank0 10010010 over [0..8] should be 001223445" $ do
      let bs = fromJust $ bitRead "10010010" :: [Word8]
      fmap (rank0 bs) [0..8] `shouldBe` [0, 0, 1, 2, 2, 3, 4, 4, 5]
    it "rank0 11011010 00000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: [Word8]
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    it "rank0 11011010 10000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 10000000" :: [Word8]
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10]
  describe "For [Word16]" $ do
    it "rank0 10010010 over [0..8] should be 001223445" $ do
      let bs = fromJust $ bitRead "10010010" :: [Word16]
      fmap (rank0 bs) [0..8] `shouldBe` [0, 0, 1, 2, 2, 3, 4, 4, 5]
    it "rank0 11011010 00000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: [Word16]
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    it "rank0 11011010 10000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 10000000" :: [Word16]
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10]
  describe "For [Word32]" $ do
    it "rank0 10010010 over [0..8] should be 001223445" $ do
      let bs = fromJust $ bitRead "10010010" :: [Word32]
      fmap (rank0 bs) [0..8] `shouldBe` [0, 0, 1, 2, 2, 3, 4, 4, 5]
    it "rank0 11011010 00000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: [Word32]
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    it "rank0 11011010 10000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 10000000" :: [Word32]
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10]
  describe "For [Word64]" $ do
    it "rank0 10010010 over [0..8] should be 001223445" $ do
      let bs = fromJust $ bitRead "10010010" :: [Word32]
      fmap (rank0 bs) [0..8] `shouldBe` [0, 0, 1, 2, 2, 3, 4, 4, 5]
    it "rank0 11011010 00000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: [Word32]
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    it "rank0 11011010 10000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 10000000" :: [Word32]
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10]
  describe "For (DV.Vector Word8)" $ do
    it "rank0 10010010 over [0..8] should be 001223445" $ do
      let bs = fromJust $ bitRead "10010010" :: DV.Vector Word8
      fmap (rank0 bs) [0..8] `shouldBe` [0, 0, 1, 2, 2, 3, 4, 4, 5]
    it "rank0 11011010 00000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: DV.Vector Word8
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    it "rank0 11011010 10000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 10000000" :: DV.Vector Word8
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10]
  describe "For (DV.Vector Word16)" $ do
    it "rank0 10010010 over [0..8] should be 001223445" $ do
      let bs = fromJust $ bitRead "10010010" :: DV.Vector Word16
      fmap (rank0 bs) [0..8] `shouldBe` [0, 0, 1, 2, 2, 3, 4, 4, 5]
    it "rank0 11011010 00000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: DV.Vector Word16
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    it "rank0 11011010 10000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 10000000" :: DV.Vector Word16
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10]
  describe "For (DV.Vector Word32)" $ do
    it "rank0 10010010 over [0..8] should be 001223445" $ do
      let bs = fromJust $ bitRead "10010010" :: DV.Vector Word32
      fmap (rank0 bs) [0..8] `shouldBe` [0, 0, 1, 2, 2, 3, 4, 4, 5]
    it "rank0 11011010 00000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: DV.Vector Word32
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    it "rank0 11011010 10000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 10000000" :: DV.Vector Word32
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10]
  describe "For (DV.Vector Word64)" $ do
    it "rank0 10010010 over [0..8] should be 001223445" $ do
      let bs = fromJust $ bitRead "10010010" :: DV.Vector Word64
      fmap (rank0 bs) [0..8] `shouldBe` [0, 0, 1, 2, 2, 3, 4, 4, 5]
    it "rank0 11011010 00000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: DV.Vector Word64
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    it "rank0 11011010 10000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 10000000" :: DV.Vector Word64
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10]
  describe "For (DVS.Vector Word8)" $ do
    it "rank0 10010010 over [0..8] should be 001223445" $ do
      let bs = fromJust $ bitRead "10010010" :: DVS.Vector Word8
      fmap (rank0 bs) [0..8] `shouldBe` [0, 0, 1, 2, 2, 3, 4, 4, 5]
    it "rank0 11011010 00000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: DVS.Vector Word8
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    it "rank0 11011010 10000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 10000000" :: DVS.Vector Word8
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10]
  describe "For (DVS.Vector Word16)" $ do
    it "rank0 10010010 over [0..8] should be 001223445" $ do
      let bs = fromJust $ bitRead "10010010" :: DVS.Vector Word16
      fmap (rank0 bs) [0..8] `shouldBe` [0, 0, 1, 2, 2, 3, 4, 4, 5]
    it "rank0 11011010 00000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 00000000" :: DVS.Vector Word16
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
    it "rank0 11011010 10000000 over [0..16]" $ do
      let bs = fromJust $ bitRead "11011010 10000000" :: DVS.Vector Word16
      fmap (rank0 bs) [0..16] `shouldBe` [0, 0, 0, 1, 1, 1, 2, 2, 3, 3, 4, 5, 6, 7, 8, 9, 10]
