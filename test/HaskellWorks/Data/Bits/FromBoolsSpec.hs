{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.FromBoolsSpec (spec) where

import           Data.Word
import           HaskellWorks.Data.Bits.FromBools
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.FromBoolsSpec" $ do
  describe "for Word8" $ do
    it "fromBool works for 0 bits" $ do
      let result = fromBools [] :: Maybe (Word8, [Bool])
      result `shouldBe` Nothing
    it "fromBool works for 4 bits" $ do
      let result = fromBools  [ True, False, True, False] :: Maybe (Word8, [Bool])
      result `shouldBe` Just (0x5, [])
    it "fromBool works for 8 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              ] :: Maybe (Word8, [Bool])
      result `shouldBe` Just (0x55, [])
    it "fromBool works for 12 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              , True, False, True, False
                              ] :: Maybe (Word8, [Bool])
      result `shouldBe` Just (0x55, [True, False, True, False])
  describe "for Word16" $ do
    it "fromBool works for 0 bits" $ do
      let result = fromBools [] :: Maybe (Word16, [Bool])
      result `shouldBe` Nothing
    it "fromBool works for 4 bits" $ do
      let result = fromBools  [ True, False, True, False
                              ] :: Maybe (Word16, [Bool])
      result `shouldBe` Just (0x5, [])
    it "fromBool works for 8 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              ] :: Maybe (Word16, [Bool])
      result `shouldBe` Just (0x55, [])
    it "fromBool works for 12 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              , True, False, True, False
                              ] :: Maybe (Word16, [Bool])
      result `shouldBe` Just (0x555, [])
    it "fromBool works for 16 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              ] :: Maybe (Word16, [Bool])
      result `shouldBe` Just (0x5555, [])
    it "fromBool works for 20 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False
                              ] :: Maybe (Word16, [Bool])
      result `shouldBe` Just (0x5555, [True, False, True, False])
  describe "for Word32" $ do
    it "fromBool works for 0 bits" $ do
      let result = fromBools [] :: Maybe (Word32, [Bool])
      result `shouldBe` Nothing
    it "fromBool works for 4 bits" $ do
      let result = fromBools  [ True, False, True, False
                              ] :: Maybe (Word32, [Bool])
      result `shouldBe` Just (0x5, [])
    it "fromBool works for 8 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              ] :: Maybe (Word32, [Bool])
      result `shouldBe` Just (0x55, [])
    it "fromBool works for 12 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              , True, False, True, False
                              ] :: Maybe (Word32, [Bool])
      result `shouldBe` Just (0x555, [])
    it "fromBool works for 20 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False
                              ] :: Maybe (Word32, [Bool])
      result `shouldBe` Just (0x55555, [])
    it "fromBool works for 32 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              ] :: Maybe (Word32, [Bool])
      result `shouldBe` Just (0x55555555, [])
    it "fromBool works for 36 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False
                              ] :: Maybe (Word32, [Bool])
      result `shouldBe` Just (0x55555555, [True, False, True, False])
  describe "for Word64" $ do
    it "fromBool works for 0 bits" $ do
      let result = fromBools [] :: Maybe (Word64, [Bool])
      result `shouldBe` Nothing
    it "fromBool works for 4 bits" $ do
      let result = fromBools  [ True, False, True, False
                              ] :: Maybe (Word64, [Bool])
      result `shouldBe` Just (0x5, [])
    it "fromBool works for 8 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              ] :: Maybe (Word64, [Bool])
      result `shouldBe` Just (0x55, [])
    it "fromBool works for 12 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              , True, False, True, False
                              ] :: Maybe (Word64, [Bool])
      result `shouldBe` Just (0x555, [])
    it "fromBool works for 20 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False
                              ] :: Maybe (Word64, [Bool])
      result `shouldBe` Just (0x55555, [])
    it "fromBool works for 32 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              ] :: Maybe (Word64, [Bool])
      result `shouldBe` Just (0x55555555, [])
    it "fromBool works for 36 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False
                              ] :: Maybe (Word64, [Bool])
      result `shouldBe` Just (0x555555555, [])
    it "fromBool works for 64 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              ] :: Maybe (Word64, [Bool])
      result `shouldBe` Just (0x5555555555555555, [])
    it "fromBool works for 68 bits" $ do
      let result = fromBools  [ True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False, True, False, True, False
                              , True, False, True, False
                              ] :: Maybe (Word64, [Bool])
      result `shouldBe` Just (0x5555555555555555, [True, False, True, False])
