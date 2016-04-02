module HaskellWorks.Data.Json.Succinct.Transform
  ( jsonToInterestBits
  , jsonToInterestBalancedParens
  ) where

import qualified Data.ByteString                      as BS
import           Data.Conduit
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Conduit.Json
import           HaskellWorks.Data.Conduit.Json.Blank
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.FromByteString

jsonToInterestBits :: [BS.ByteString] -> [Bool]
jsonToInterestBits json = bitShown (fromByteString x)
  where x = BS.concat $ runListConduit json $ blankJson =$= blankedJsonToInterestBits

jsonToInterestBalancedParens :: [BS.ByteString] -> [Bool]
jsonToInterestBalancedParens json = runListConduit json $ blankJson =$= blankedJsonToBalancedParens
