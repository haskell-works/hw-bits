{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}


module HaskellWorks.Data.Json.Succinct.Cursor.InterestBitsSpec(spec) where

import qualified Data.ByteString                                     as BS
import           Data.String
import qualified Data.Vector.Storable                                as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Json.Succinct.Cursor.BlankedJson
import           HaskellWorks.Data.Json.Succinct.Cursor.InterestBits
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}

interestBitsOf :: FromBlankedJson (JsonInterestBits a) => BS.ByteString -> a
interestBitsOf = getJsonInterestBits . fromBlankedJson . fromByteString

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.Cursor.InterestBitsSpec" $ do
  it "Evaluating interest bits" $ do
    (interestBitsOf ""  :: BitShown (DVS.Vector Word64)) `shouldBe` fromString ""
    (interestBitsOf "2" :: BitShown (DVS.Vector Word64)) `shouldBe` fromString "1"
