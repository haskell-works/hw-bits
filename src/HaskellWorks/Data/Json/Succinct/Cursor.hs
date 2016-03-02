{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module HaskellWorks.Data.Json.Succinct.Cursor where

import qualified Data.ByteString.Lazy.Char8                   as BS
import           Data.String
import qualified Data.Vector                                  as DV
import           Data.Word
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens
import           HaskellWorks.Data.Succinct.RankSelect.Simple

data JsonCursor v = JsonCursor
  { position       :: Position
  , balancedParens :: SimpleBalancedParens v
  , interests      :: Simple v
  }

instance IsString (JsonCursor (DV.Vector Word64)) where
  fromString :: String -> JsonCursor (DV.Vector Word64)
  fromString s = JsonCursor
    { position        = 0
    , balancedParens  = undefined
    , interests       = undefined
    }
    where bs = BS.pack s :: BS.ByteString
