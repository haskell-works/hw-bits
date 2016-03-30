{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.SuccinctSpec where

import           Data.Maybe
import           HaskellWorks.Data.Bits.BitRead
import           HaskellWorks.Data.Conduit.Json
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.Json.Succinct
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Json.SuccinctSpec" $ do
  describe "When running markerToByteString" $ do
    it "Marker at zero gives \"\1\"" $
      runListConduit [0] markerToByteString `shouldBe` ["\1"]
    it "Marker at [0, 1, 2, 3, 4, 5, 6, 7] gives \"\\255\"" $
      runListConduit [0, 1, 2, 3, 4, 5, 6, 7] markerToByteString `shouldBe` ["\255"]
    it "Marker at [0, 9, 18, 27, 36] gives \"\\255\"" $
      runListConduit [0, 9, 18, 27, 36] markerToByteString `shouldBe` ["\1", "\2", "\4", "\8", "\16"]
    it "Marker at [0, 9, 27, 36] gives \"\\255\"" $
      runListConduit [0, 9, 27, 36] markerToByteString `shouldBe` ["\1", "\2", "\0", "\8", "\16"]
    it "Marker at [0, 36] gives \"\\255\"" $
      runListConduit [0, 36] markerToByteString `shouldBe` ["\1", "\0", "\0", "\0", "\16"]
  describe "jsonToInterestBalancedParens" $ do
    it "produces the correct interest bits for {\"field\": 1}" $
      jsonToInterestBalancedParens ["{\"field\": 1}"] `shouldBe` fromJust (bitRead "110100")
  describe "jsonToInterestBits" $ do
    it "produces the correct interest bits for {\"field\": 1}" $
      jsonToInterestBits ["{\"field\": 1}"] `shouldBe` fromJust (bitRead "1100000000100000")
