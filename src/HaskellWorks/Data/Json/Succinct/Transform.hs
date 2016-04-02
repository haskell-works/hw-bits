module HaskellWorks.Data.Json.Succinct.Transform
  ( jsonToInterestBits
  , jsonToInterestBalancedParens
  ) where

import qualified Data.ByteString                      as BS
import           Data.Conduit
import           HaskellWorks.Data.Conduit.Json
import           HaskellWorks.Data.Conduit.Json.Blank
import           HaskellWorks.Data.Conduit.List

jsonToInterestBits :: [BS.ByteString] -> [Bool]
jsonToInterestBits json = runListConduit json $
  textToJsonToken =$= jsonToken2Markers =$= markerToByteString =$= byteStringToBits

jsonToInterestBalancedParens :: [BS.ByteString] -> [Bool]
jsonToInterestBalancedParens json = runListConduit json $
  blankJson =$= blankedJsonToBalancedParens
