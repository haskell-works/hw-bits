{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.BitWiseSpec (spec) where

import qualified Data.Bits                      as B
import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.SuccinctSpec" $ do
  it "popCount1 for Word8 matches Data.Bits implementation" $ property $
    \(w :: Word8 ) -> popCount1 w == fromIntegral (B.popCount w)
  it "popCount1 for Word16 matches Data.Bits implementation" $ property $
    \(w :: Word16) -> popCount1 w == fromIntegral (B.popCount w)
  it "popCount1 for Word32 matches Data.Bits implementation" $ property $
    \(w :: Word32) -> popCount1 w == fromIntegral (B.popCount w)
  it "popCount1 for Word64 matches Data.Bits implementation" $ property $
    \(w :: Word64) -> popCount1 w == fromIntegral (B.popCount w)
