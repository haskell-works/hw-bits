{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE    ScopedTypeVariables           #-}

module HaskellWorks.Data.Bits.PackedVector.PackedVector64Spec (spec) where

import           Data.Word
import           HaskellWorks.Data.AtIndex
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.PackedVector.PackedVector64
import           HaskellWorks.Data.Positioning
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

subWordSize :: Count -> Gen Count
subWordSize maxWordSize = choose (1, maxWordSize)

word64OfSize :: Count -> Gen Word64
word64OfSize sz = choose (0, 1 .<. fromIntegral sz - 1)

listLen :: Gen Int
listLen = choose (1, 128)

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.PackedVector.PackedVector64Spec" $ do
  it "Round trip from list" $
    forAll (subWordSize 64) $ \wSize ->
      forAll listLen $ \len ->
        forAll (vectorOf len (word64OfSize wSize)) $ \ws ->
          toList (fromList wSize ws) `shouldBe` ws
  it "Round trip from list" $
    forAll (subWordSize 64) $ \wSize ->
      forAll (choose (1, 32 :: Int)) $ \len ->
        forAll (vectorOf len (word64OfSize wSize)) $ \ws ->
          forAll (choose (0, fromIntegral len - 1 :: Position)) $ \i ->
            (fromList wSize ws !!! i) `shouldBe` (ws !!! i)
