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


-- firstChild :: v -> Position -> Position
-- firstChild _ = (+ 1)

-- nextSibling :: (BalancedParens v) => v -> Position -> Position
-- nextSibling v p = findClose v p + 1

-- parent :: (BalancedParens v) => v -> Position -> Position
-- parent = enclose

-- depth :: (BalancedParens v, Rank1 v) => v -> Position -> Count
-- depth = rank1

-- subtreeSize :: (BalancedParens v) => v -> Position -> Count
-- subtreeSize v p = toCount ((findClose v p - p + 1) `quot` 2)
