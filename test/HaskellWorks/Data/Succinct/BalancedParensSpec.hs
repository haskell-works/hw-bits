{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.BalancedParensSpec where

import           Data.Maybe
import           Data.Word
import           HaskellWorks.Data.Bits.BitString
import           HaskellWorks.Data.Succinct.BalancedParens
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.SuccinctSpec" $ do
  describe "For (()(()())) 0010010111" $ do
    let bs = SimpleBalancedParens (91 :: Word64)
    it "Test 1a" $ findClose bs 0 `shouldBe` 9
    it "Test 1b" $ findClose bs 1 `shouldBe` 2
    it "Test 2a" $ findOpen  bs 9 `shouldBe` 0
    it "Test 2b" $ findOpen  bs 2 `shouldBe` 1
    it "Test 3a" $ enclose   bs 1 `shouldBe` 0
    it "Test 3b" $ enclose   bs 6 `shouldBe` 3
  describe "For (()(()())) 1101101000" $ do
    let bs = SimpleBalancedParens (fromJust (fromBitString "1101101000") :: [Bool])
    it "Test 1a" $ findClose bs 0 `shouldBe` 9
    it "Test 1b" $ findClose bs 1 `shouldBe` 2
    it "Test 2a" $ findOpen  bs 9 `shouldBe` 0
    it "Test 2b" $ findOpen  bs 2 `shouldBe` 1
    it "Test 3a" $ enclose   bs 1 `shouldBe` 0
    it "Test 3b" $ enclose   bs 6 `shouldBe` 3

