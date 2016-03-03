{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module HaskellWorks.Data.Json.Succinct.Cursor where

import qualified Data.ByteString.Char8                     as BS
import           Data.String
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Json.Succinct.Transform
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens as BP
import           HaskellWorks.Data.Succinct.RankSelect

class TreeCursor k where
  firstChild :: k -> k
  nextSibling :: k -> k
  parent :: k -> k
  depth :: k -> Count
  subtreeSize :: k -> Count

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

instance TreeCursor (JsonCursor String [Bool]) where
  firstChild  k = k { position = select1 (interests k) (toCount (BP.firstChild  (balancedParens k) (position k))) }
  nextSibling k = k { position = select1 (interests k) (toCount (BP.nextSibling (balancedParens k) (position k))) }
  parent      k = k { position = select1 (interests k) (toCount (BP.parent      (balancedParens k) (position k))) }
  depth       k = BP.depth (balancedParens k) (position k)
  subtreeSize k = BP.subtreeSize (balancedParens k) (position k)

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
