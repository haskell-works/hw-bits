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
spec = describe "HaskellWorks.Data.Succinct.BalancedParensSpec" $ do
  describe "For (()(()())) 1101101000" $ do
    let bs = SimpleBalancedParens (91 :: Word64)
    it "Test 1a" $ findClose bs  1 `shouldBe` 10
    it "Test 1b" $ findClose bs  2 `shouldBe`  3
    it "Test 2a" $ findOpen  bs 10 `shouldBe`  1
    it "Test 2b" $ findOpen  bs  3 `shouldBe`  2
    it "Test 3a" $ enclose   bs  2 `shouldBe`  1
    it "Test 3b" $ enclose   bs  7 `shouldBe`  4
  describe "For (()(()())) 1101101000" $ do
    let bs = SimpleBalancedParens (fromJust (fromBitString "1101101000") :: [Bool])
    it "Test 1a" $ findClose bs  1 `shouldBe` 10
    it "Test 1b" $ findClose bs  2 `shouldBe`  3
    it "Test 1b" $ findClose bs  3 `shouldBe`  3
    it "Test 1b" $ findClose bs  4 `shouldBe`  9
    it "Test 2a" $ findOpen  bs 10 `shouldBe`  1
    it "Test 2b" $ findOpen  bs  3 `shouldBe`  2
    it "Test 3a" $ enclose   bs  2 `shouldBe`  1
    it "Test 3b" $ enclose   bs  7 `shouldBe`  4
    it "firstChild 1" $ firstChild bs 1 `shouldBe` 2
    it "firstChild 4" $ firstChild bs 4 `shouldBe` 5
    it "nextSibling 2" $ nextSibling bs 2 `shouldBe` 4
    it "nextSibling 5" $ nextSibling bs 5 `shouldBe` 7
    it "parent 2" $ parent bs 2 `shouldBe` 1
    it "parent 5" $ parent bs 5 `shouldBe` 4
    it "depth  1" $ depth bs  1 `shouldBe` 1
    it "depth  2" $ depth bs  2 `shouldBe` 2
    it "depth  3" $ depth bs  3 `shouldBe` 2
    it "depth  4" $ depth bs  4 `shouldBe` 2
    it "depth  5" $ depth bs  5 `shouldBe` 3
    it "depth  6" $ depth bs  6 `shouldBe` 3
    it "depth  7" $ depth bs  7 `shouldBe` 3
    it "depth  8" $ depth bs  8 `shouldBe` 3
    it "depth  9" $ depth bs  9 `shouldBe` 2
    it "depth 10" $ depth bs 10 `shouldBe` 1

-- depth :: (BalancedParens v, Rank1 v) => v -> Position -> Count
-- depth = rank1

-- subtreeSize :: (BalancedParens v) => v -> Position -> Count
-- subtreeSize v p = toCount ((findClose v p - p + 1) `quot` 2)
