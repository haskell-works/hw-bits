{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module HaskellWorks.Data.Json.Succinct.Cursor where

import qualified Data.ByteString.Char8                     as BS
import           Data.String
import           Debug.Trace
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
  , cursorRank     :: Count
  }
  deriving (Eq, Show)

instance IsString (JsonCursor String [Bool]) where
  fromString :: String -> JsonCursor String [Bool]
  fromString s = JsonCursor
    { cursorText      = s
    , cursorRank           = 1
    , balancedParens  = SimpleBalancedParens (jsonToInterestBalancedParens [bs])
    , interests       = Simple interests'
    }
    where bs          = BS.pack s :: BS.ByteString
          interests'  = jsonToInterestBits [bs]

instance TreeCursor (JsonCursor String [Bool]) where
  firstChild  k = k { cursorRank = rank1 (balancedParens k) (BP.firstChild   (balancedParens k) (select1 (balancedParens k) (cursorRank k))) }
  nextSibling k = k { cursorRank = rank1 (balancedParens k) (BP.nextSibling  (balancedParens k) (select1 (balancedParens k) (cursorRank k))) }
  parent      k = k { cursorRank = undefined }-- BP.parent       (balancedParens k) (cursorRank k) }
  depth       k = undefined -- BP.depth (balancedParens k) (cursorRank k)
  subtreeSize k = undefined -- BP.subtreeSize (balancedParens k) (cursorRank k)

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
  _   -> error "Invalid JsonCursor cursorRank"
  where c = cursorText k !! fromIntegral (select1 (interests k) (cursorRank k) - 1)
