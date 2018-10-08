{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.Log2Spec (spec) where

import Data.Word
import HaskellWorks.Data.Bits.Log2
import Test.Hspec
import Test.QuickCheck

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.Log2Spec" $ do
  it "Log2 Word64" $ property $
    \(NonZero (w :: Word64)) -> log2 w == floor (log (fromIntegral w) / log 2 :: Double)
  it "Log2 Word32" $ property $
    \(NonZero (w :: Word32)) -> log2 w == floor (log (fromIntegral w) / log 2 :: Double)
