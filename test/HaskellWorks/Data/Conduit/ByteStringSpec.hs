{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Conduit.ByteStringSpec (spec) where

import           HaskellWorks.Data.Conduit.ByteString
import           HaskellWorks.Data.Conduit.List
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Conduit.ByteStringSpec" $ do
  it "Can rechunk bytestrings" $ do
    runListConduit (rechunk 1) [] `shouldBe` []
    runListConduit (rechunk 1) ["0", "1", "2", "3", "4", "5"] `shouldBe` ["0", "1", "2", "3", "4", "5"]
    runListConduit (rechunk 2) ["0", "1", "2", "3", "4", "5"] `shouldBe` ["01", "23", "45"]
    runListConduit (rechunk 3) ["0", "1", "2", "3", "4", "5"] `shouldBe` ["012", "345"]
    runListConduit (rechunk 4) ["0", "1", "2", "3", "4", "5"] `shouldBe` ["0123", "45"]
    runListConduit (rechunk 5) ["0", "1", "2", "3", "4", "5"] `shouldBe` ["01234", "5"]
    runListConduit (rechunk 6) ["0", "1", "2", "3", "4", "5"] `shouldBe` ["012345"]
    runListConduit (rechunk 7) ["0", "1", "2", "3", "4", "5"] `shouldBe` ["012345"]
    runListConduit (rechunk 1) ["01", "23", "", "45", "67"] `shouldBe` ["0", "1", "2", "3", "4", "5", "6", "7"]
    runListConduit (rechunk 2) ["01", "23", "", "45", "67"] `shouldBe` ["01", "23", "45", "67"]
    runListConduit (rechunk 3) ["01", "23", "", "45", "67"] `shouldBe` ["012", "345", "67"]
    runListConduit (rechunk 4) ["01", "23", "", "45", "67"] `shouldBe` ["0123", "4567"]
    runListConduit (rechunk 5) ["01", "23", "", "45", "67"] `shouldBe` ["01234", "567"]
    runListConduit (rechunk 6) ["01", "23", "", "45", "67"] `shouldBe` ["012345", "67"]
    runListConduit (rechunk 7) ["01", "23", "", "45", "67"] `shouldBe` ["0123456", "7"]
    runListConduit (rechunk 8) ["01", "23", "", "45", "67"] `shouldBe` ["01234567"]
    runListConduit (rechunk 9) ["01", "23", "", "45", "67"] `shouldBe` ["01234567"]
