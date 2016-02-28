{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.SuccinctSpec where

import           HaskellWorks.Data.Conduit.Json
import           HaskellWorks.Data.Json.Succinct
import           Test.Hspec

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
  describe "moo" $ do
    it "foo" $
      jsonToInterestBalancedParens ["{\"field\": 1}"] `shouldBe` [True, True, False, True, False, False]
    it "moo" $
      jsonToInterestBits ["{\"field\": 1}"] `shouldBe`
        [True, True, False, False, False, False, False, False, True, False, True, True, False, False, False, False]
