module HaskellWorks.Data.Bits.Broadword.Word64Spec
  ( spec
  ) where

import Data.Word
import HaskellWorks.Data.Bits.Broadword.Word64
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

μ :: Word64 -> Word64
μ k = (2 ^ ((2 ^ 64)) - 1) `div` (2 ^ (2 ^ k) + 1)

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.Broadword.Word64Spec" $ do
  describe "μ" $ do
    it "μ0" $ requireTest $ μ0 === μ 0
    it "μ1" $ requireTest $ μ1 === μ 1
    it "μ2" $ requireTest $ μ2 === μ 2
    it "μ3" $ requireTest $ μ3 === μ 3
    it "μ4" $ requireTest $ μ4 === μ 4
    it "μ5" $ requireTest $ μ5 === μ 5
    it "μ6" $ requireTest $ μ6 === μ 6
