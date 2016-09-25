{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE    ScopedTypeVariables           #-}

module HaskellWorks.Data.Bits.PackedVector.InternalSpec (spec) where

import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.PackedVector.Internal
import           HaskellWorks.Data.Positioning
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

subWordSize :: Count -> Gen Count
subWordSize maxWordSize = choose (1, maxWordSize)

word8OfSize :: Count -> Gen Word8
word8OfSize sz = choose (0, 1 .<. fromIntegral sz - 1)

word64OfSize :: Count -> Gen Word64
word64OfSize sz = choose (0, 1 .<. fromIntegral sz - 1)

listLen :: Gen Int
listLen = choose (1, 128)

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.PackedVector.InternalSpec" $ do
  it "PackedVector Word8" $
    forAll (subWordSize 8) $ \wSize ->
      forAll (choose (1, 3)) $ \len ->
        forAll (vectorOf len (word8OfSize wSize)) $ \ws ->
          unpackBits (length ws) wSize (packBits wSize ws) `shouldBe` ws
  it "PackedVector Word64" $
    forAll (subWordSize 64) $ \wSize ->
      forAll listLen $ \len ->
        forAll (vectorOf len (word64OfSize wSize)) $ \ws ->
          unpackBits (length ws) wSize (packBits wSize ws) `shouldBe` ws
