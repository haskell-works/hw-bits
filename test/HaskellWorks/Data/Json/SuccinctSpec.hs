{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.SuccinctSpec where

import           HaskellWorks.Data.Json.Succinct
import           Test.Hspec

spec :: Spec
spec = describe "HaskellWorks.Data.Json.SuccinctSpec" $ do
  describe "moo" $ do
    it "foo" $
      jsonToInterestBalancedParens ["{\"field\": 1}"] `shouldBe` [True, True, False, True, False, False]
