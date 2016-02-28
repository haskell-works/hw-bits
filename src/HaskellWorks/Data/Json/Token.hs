module HaskellWorks.Data.Json.Token (JsonToken(..), JsonTokenLike(..)) where

class JsonTokenLike j where
  jsonTokenBraceL     :: j
  jsonTokenBraceR     :: j
  jsonTokenBracketL   :: j
  jsonTokenBracketR   :: j
  jsonTokenComma      :: j
  jsonTokenColon      :: j
  jsonTokenWhitespace :: j
  jsonTokenString     :: String -> j
  jsonTokenBoolean    :: Bool -> j
  jsonTokenNumber     :: Double -> j
  jsonTokenNull       :: j

data JsonToken
  = JsonTokenBraceL
  | JsonTokenBraceR
  | JsonTokenBracketL
  | JsonTokenBracketR
  | JsonTokenComma
  | JsonTokenColon
  | JsonTokenWhitespace
  | JsonTokenString String
  | JsonTokenBoolean Bool
  | JsonTokenNumber Double
  | JsonTokenNull
  deriving (Eq, Show)

instance JsonTokenLike JsonToken where
  jsonTokenBraceL     = JsonTokenBraceL
  jsonTokenBraceR     = JsonTokenBraceR
  jsonTokenBracketL   = JsonTokenBracketL
  jsonTokenBracketR   = JsonTokenBracketR
  jsonTokenComma      = JsonTokenComma
  jsonTokenColon      = JsonTokenColon
  jsonTokenWhitespace = JsonTokenWhitespace
  jsonTokenString     = JsonTokenString
  jsonTokenBoolean    = JsonTokenBoolean
  jsonTokenNumber     = JsonTokenNumber
  jsonTokenNull       = JsonTokenNull
