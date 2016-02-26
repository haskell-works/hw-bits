{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Succinct.BalancedParensSpec where

import           Data.Word
import           HaskellWorks.Data.Succinct.BalancedParens
import           Test.Hspec

spec :: Spec
spec = describe "HaskellWorks.Data.SuccinctSpec" $ do
  -- (()(()())) 0010010111
  let bs = SimpleBalancedParens (0x3A4 :: Word64)
  it "Test 1a" $ findClose bs 0 `shouldBe` 9
  it "Test 1b" $ findClose bs 1 `shouldBe` 2
  it "Test 2a" $ findOpen  bs 9 `shouldBe` 0
  it "Test 2b" $ findOpen  bs 2 `shouldBe` 1
  it "Test 3a" $ enclose   bs 1 `shouldBe` 0
  it "Test 3b" $ enclose   bs 6 `shouldBe` 3
