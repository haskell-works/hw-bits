{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.UnmatchedSpec (spec) where

import           Data.Vector.Storable                 as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.FixedBitSize
import           HaskellWorks.Data.Bits.Unmatched
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.UnmatchedSpec" $ do
  describe "For Word8" $ do
    it "umatchedL0 concatentation" $
      property $ \(a :: Word8) (b :: Word8) ->
        let c :: Word16 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedL0 a + ((unmatchedL0 b - unmatchedR1 a) `max` 0) `shouldBe` unmatchedL0 c
    it "umatchedL1 concatentation" $
      property $ \(a :: Word8) (b :: Word8) ->
        let c :: Word16 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedL1 a + ((unmatchedL1 b - unmatchedR0 a) `max` 0) `shouldBe` unmatchedL1 c
    it "umatchedR0 concatentation" $
      property $ \(a :: Word8) (b :: Word8) ->
        let c :: Word16 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedR0 b + ((unmatchedR0 a - unmatchedL1 b) `max` 0) `shouldBe` unmatchedR0 c
    it "umatchedR1 concatentation" $
      property $ \(a :: Word8) (b :: Word8) ->
        let c :: Word16 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedR1 b + ((unmatchedR1 a - unmatchedL0 b) `max` 0) `shouldBe` unmatchedR1 c
  describe "For Word16" $ do
    it "umatchedL0 concatentation" $
      property $ \(a :: Word16) (b :: Word16) ->
        let c :: Word32 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedL0 a + ((unmatchedL0 b - unmatchedR1 a) `max` 0) `shouldBe` unmatchedL0 c
    it "umatchedL1 concatentation" $
      property $ \(a :: Word16) (b :: Word16) ->
        let c :: Word32 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedL1 a + ((unmatchedL1 b - unmatchedR0 a) `max` 0) `shouldBe` unmatchedL1 c
    it "umatchedR0 concatentation" $
      property $ \(a :: Word16) (b :: Word16) ->
        let c :: Word32 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedR0 b + ((unmatchedR0 a - unmatchedL1 b) `max` 0) `shouldBe` unmatchedR0 c
    it "umatchedR1 concatentation" $
      property $ \(a :: Word16) (b :: Word16) ->
        let c :: Word32 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedR1 b + ((unmatchedR1 a - unmatchedL0 b) `max` 0) `shouldBe` unmatchedR1 c
  describe "For Word32" $ do
    it "umatchedL0 concatentation" $
      property $ \(a :: Word32) (b :: Word32) ->
        let c :: Word64 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedL0 a + ((unmatchedL0 b - unmatchedR1 a) `max` 0) `shouldBe` unmatchedL0 c
    it "umatchedL1 concatentation" $
      property $ \(a :: Word32) (b :: Word32) ->
        let c :: Word64 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedL1 a + ((unmatchedL1 b - unmatchedR0 a) `max` 0) `shouldBe` unmatchedL1 c
    it "umatchedR0 concatentation" $
      property $ \(a :: Word32) (b :: Word32) ->
        let c :: Word64 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedR0 b + ((unmatchedR0 a - unmatchedL1 b) `max` 0) `shouldBe` unmatchedR0 c
    it "umatchedR1 concatentation" $
      property $ \(a :: Word32) (b :: Word32) ->
        let c :: Word64 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedR1 b + ((unmatchedR1 a - unmatchedL0 b) `max` 0) `shouldBe` unmatchedR1 c
  describe "For (DVS.Vector Word8)" $ do
    it "umatchedL0 concatentation" $
      property $ \(a :: Word8) (b :: Word8) ->
        let c = DVS.fromList [a, b] in
        let d :: Word16 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedL0 d `shouldBe` unmatchedL0 c
    it "umatchedL1 concatentation" $
      property $ \(a :: Word8) (b :: Word8) ->
        let c = DVS.fromList [a, b] in
        let d :: Word16 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedL1 d `shouldBe` unmatchedL1 c
    it "umatchedR0 concatentation" $
      property $ \(a :: Word8) (b :: Word8) ->
        let c = DVS.fromList [a, b] in
        let d :: Word16 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedR0 d `shouldBe` unmatchedR0 c
    it "umatchedR1 concatentation" $
      property $ \(a :: Word8) (b :: Word8) ->
        let c = DVS.fromList [a, b] in
        let d :: Word16 = (fromIntegral a) .|. (fromIntegral b .<. fixedBitSize b) in
        unmatchedR1 d `shouldBe` unmatchedR1 c
