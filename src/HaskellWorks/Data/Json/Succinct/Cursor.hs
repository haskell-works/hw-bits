{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module HaskellWorks.Data.Json.Succinct.Cursor where

import qualified Data.ByteString.Char8                     as BS
import           Data.String
import           HaskellWorks.Data.Json.Succinct.Transform
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens
import           HaskellWorks.Data.Succinct.RankSelect

data JsonCursor t v = JsonCursor
  { cursorText     :: t
  , interests      :: Simple v
  , balancedParens :: SimpleBalancedParens v
  , position       :: Position
  }
  deriving (Eq, Show)

instance IsString (JsonCursor String [Bool]) where
  fromString :: String -> JsonCursor String [Bool]
  fromString s = JsonCursor
    { cursorText      = s
    , position        = select True interests' 1
    , balancedParens  = SimpleBalancedParens (jsonToInterestBalancedParens [bs])
    , interests       = Simple interests'
    }
    where bs          = BS.pack s :: BS.ByteString
          interests'  = jsonToInterestBits [bs]

data JsonCursorType
  = JsonCursorArray
  | JsonCursorBool
  | JsonCursorNull
  | JsonCursorNumber
  | JsonCursorObject
  | JsonCursorString
  deriving (Eq, Show)

jsonCursorType :: JsonCursor String [Bool] -> JsonCursorType
jsonCursorType k = case c of
  '[' -> JsonCursorArray
  't' -> JsonCursorBool
  'f' -> JsonCursorBool
  '0' -> JsonCursorNumber
  '1' -> JsonCursorNumber
  '2' -> JsonCursorNumber
  '3' -> JsonCursorNumber
  '4' -> JsonCursorNumber
  '5' -> JsonCursorNumber
  '6' -> JsonCursorNumber
  '7' -> JsonCursorNumber
  '8' -> JsonCursorNumber
  '9' -> JsonCursorNumber
  '+' -> JsonCursorNumber
  '-' -> JsonCursorNumber
  'n' -> JsonCursorNull
  '{' -> JsonCursorObject
  '"' -> JsonCursorString
  _   -> error "Invalid JsonCursor position"
  where c = cursorText k !! fromIntegral (position k - 1)
