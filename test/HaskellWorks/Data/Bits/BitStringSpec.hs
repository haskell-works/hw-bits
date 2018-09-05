{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.BitStringSpec (spec) where

import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.Bits.Gen as G
import qualified Hedgehog.Gen               as G
import qualified Hedgehog.Range             as R

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.BitStringSpec" $ do
  describe "for popCount0" $ do
    it "for Word8 matches Data.Bits implementation" $ requireProperty $ do
      bss       <- forAll $ G.bitstring (R.linear 0 1024) (G.word8 R.constantBounded)
      comp (comp bss) === bss
