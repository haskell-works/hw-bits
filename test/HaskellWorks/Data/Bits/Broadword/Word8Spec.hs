{-# LANGUAGE BinaryLiterals      #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.Broadword.Word8Spec where

import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.Bits.Broadword.Word8 as W8
import qualified Hedgehog.Gen                           as G
import qualified Hedgehog.Range                         as R

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.Broadword.Word8" $ do
  it "kBitDiff" $ requireProperty $ do
    a <- forAll $ G.word8 (R.linear 0 0xff)
    b <- forAll $ G.word8 (R.linear 0 0xff)

    W8.kBitDiff 8 a b === a - b
  it "kBitDiffPos" $ requireProperty $ do
    a <- forAll $ G.word8 (R.linear 0 0x7f)
    b <- forAll $ G.word8 (R.linear 0 0x7f)

    if a > b
      then W8.kBitDiffPos 8 a b === a - b
      else W8.kBitDiffPos 8 a b === 0
