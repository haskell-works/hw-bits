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
