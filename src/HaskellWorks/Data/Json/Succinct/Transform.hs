module HaskellWorks.Data.Json.Succinct.Transform
  ( jsonToInterestBits
  , jsonToInterestBalancedParens
  ) where

import qualified Data.ByteString                as BS
import           Data.Conduit
import           HaskellWorks.Data.Conduit.Json

jsonToInterestBits :: [BS.ByteString] -> [Bool]
jsonToInterestBits json = runListConduit json $
  textToJsonToken =$= jsonToken2Markers =$= markerToByteString =$= byteStringToBits

jsonToInterestBalancedParens :: [BS.ByteString] -> [Bool]
jsonToInterestBalancedParens json = runListConduit json $
  textToJsonToken =$= jsonToken2BalancedParens
