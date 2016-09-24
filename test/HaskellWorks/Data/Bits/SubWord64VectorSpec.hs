{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE    ScopedTypeVariables           #-}
{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

module HaskellWorks.Data.Bits.SubWord64VectorSpec (spec) where

import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.SubWord64Vector
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

subWordSize :: Int -> Gen Int
subWordSize maxWordSize = choose (1, maxWordSize)

word64OfSize :: Int -> Gen Word64
word64OfSize sz = choose (0, 1 .<. fromIntegral sz - 1)

listLen :: Gen Int
listLen = choose (1, 128)

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.SubWord64VectorSpec" $ do
  it "SubWord64Vector Word64" $
    forAll (subWordSize 64) $ \wSize ->
      forAll listLen $ \len ->
        forAll (vectorOf len (word64OfSize wSize)) $ \ws ->
          toList (fromList (fromIntegral wSize) ws) `shouldBe` ws
