{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module HaskellWorks.Data.Json.Succinct.Cursor where

import qualified Data.ByteString.Char8                     as BS
import           Data.String
import           HaskellWorks.Data.Json.Succinct.Transform
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens
import           HaskellWorks.Data.Succinct.RankSelect

data JsonCursor v = JsonCursor
  { position       :: Position
  , balancedParens :: SimpleBalancedParens v
  , interests      :: Simple v
  }

instance IsString (JsonCursor [Bool]) where
  fromString :: String -> JsonCursor [Bool]
  fromString s = JsonCursor
    { position        = select True interests' 1
    , balancedParens  = SimpleBalancedParens (jsonToInterestBalancedParens [bs])
    , interests       = Simple interests'
    }
    where bs          = BS.pack s :: BS.ByteString
          interests'  = jsonToInterestBits [bs]
