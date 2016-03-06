{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module HaskellWorks.Data.Json.Tokenize.Parsec
    (

    ) where

import           Control.Applicative
import           Control.Monad
-- import qualified Data.Attoparsec.ByteString                as ABS
-- import qualified Data.Attoparsec.ByteString.Char8          as BC
-- import qualified Data.Attoparsec.Combinator                as AC
-- import qualified Data.Attoparsec.Internal                  as I
-- import qualified Data.Attoparsec.Types                     as T
import           Data.Bits
import           Data.ByteString              (ByteString)
import qualified Data.ByteString.Internal     as BI
import           Data.Char
import           Data.MonoTraversable
import           Data.String
import qualified Data.Vector.Storable         as DVS
import           Data.Word8
-- import           HaskellWorks.Data.Attoparsec.Final.IsChar
-- import           HaskellWorks.Data.Attoparsec.Final.Parser as AFP
import           HaskellWorks.Data.Json.Token
import           Text.Parsec
import           Text.Parsec.String


instance (Monad m, DVS.Storable a) => Stream (DVS.Vector a) m a where
  uncons v | DVS.null v = return Nothing
           | otherwise = return (Just (DVS.head v, DVS.tail v))

isHexDigitChar :: Char -> Bool
isHexDigitChar c =
  '0' <= c && c <= '9' ||
  'a' <= c && c <= 'z' ||
  'A' <= c && c <= 'Z'

-- satisfyChar :: (Char -> Bool) -> VW8Parser Char
-- satisfyChar p = do
--   w <- token (0 :: Word8)
--   return ' '

mchar :: Char -> VW8Parser Integer
mchar c = undefined

-- hexDigitNumeric :: Parser t Int
-- hexDigitNumeric = do
--   c <- satisfyChar (\c -> '0' <= c && c <= '9')
--   return $ ord c - ord '0'

-- tokenPrim  :: Stream s m t
-- => (t -> String)
-- Token pretty-printing function.
-- -> (SourcePos -> t -> s -> SourcePos)
-- Next position calculating function.
-- -> (t -> Maybe a)
-- Matching function for the token to parse.
-- -> ParsecT s u m a

type VW8Parser a = ParsecT (DVS.Vector Word8) () IO a

pmain :: VW8Parser Integer
pmain = undefined

satisfyChar :: VW8Parser Char
satisfyChar = tokenPrim
  (\t -> "moo")
  (\pos t s -> pos)
  (\t -> Just ' ')

-- runInterestParse :: DVS.Vector Word8 -> IO (Either ParseError Char)
-- runInterestParse = runParserT (char ' ') () "No source"

-- hexDigitAlphaLower :: Parser t Int
-- hexDigitAlphaLower = do
--   c <- satisfyChar (\c -> 'a' <= c && c <= 'z')
--   return $ ord c - ord 'a' + 10

-- hexDigitAlphaUpper :: Parser t Int
-- hexDigitAlphaUpper = do
--   c <- satisfyChar (\c -> 'A' <= c && c <= 'Z')
--   return $ ord c - ord 'A' + 10

-- hexDigit :: Parser t Int
-- hexDigit = hexDigitNumeric <|> hexDigitAlphaLower <|> hexDigitAlphaUpper

-- parseJsonTokenString :: (JsonTokenLike j, Parser t, Alternative (Parser t), IsString t) => Parser t j
-- parseJsonTokenString = do
--   string "\""
--   value <- many (verbatimChar <|> escapedChar <|> escapedCode)
--   string "\""
--   return $ jsonTokenString value
--   where
--     verbatimChar  = satisfyChar (BC.notInClass "\"\\") <?> "invalid string character"
--     escapedChar   = string "\\" >> BC.choice (zipWith escapee chars replacements)
--     escapedCode   = do
--       string "\\u"
--       a <- hexDigit
--       b <- hexDigit
--       c <- hexDigit
--       d <- hexDigit
--       return $ chr $ a `shift` 24 .|. b `shift` 16 .|. c `shift` 8 .|. d
--     escapee c r   = try $ char '\\' >> char c >> return r
--     chars         = [ 'b',  'n',  'f',  'r',  't', '\\', '\'', '/']
--     replacements  = ['\b', '\n', '\f', '\r', '\t', '\\', '\'', '/']

-- parseJsonTokenBraceL :: (JsonTokenLike j, Parser t, IsString t) => Parser t j
-- parseJsonTokenBraceL = string "{" >> return jsonTokenBraceL

-- parseJsonTokenBraceR :: (JsonTokenLike j, Parser t, IsString t) => Parser t j
-- parseJsonTokenBraceR = string "}" >> return jsonTokenBraceR

-- parseJsonTokenBracketL :: (JsonTokenLike j, Parser t, IsString t) => Parser t j
-- parseJsonTokenBracketL = string "[" >> return jsonTokenBracketL

-- parseJsonTokenBracketR :: (JsonTokenLike j, Parser t, IsString t) => Parser t j
-- parseJsonTokenBracketR = string "]" >> return jsonTokenBracketR

-- parseJsonTokenComma :: (JsonTokenLike j, Parser t, IsString t) => Parser t j
-- parseJsonTokenComma = string "," >> return jsonTokenComma

-- parseJsonTokenColon :: (JsonTokenLike j, Parser t, IsString t) => Parser t j
-- parseJsonTokenColon = string ":" >> return jsonTokenColon

-- parseJsonTokenWhitespace :: (JsonTokenLike j, Parser t, IsString t) => Parser t j
-- parseJsonTokenWhitespace = do
--   AC.many1' $ BC.choice [string " ", string "\t", string "\n", string "\r"]
--   return $ jsonTokenWhitespace

-- parseJsonTokenNull :: (JsonTokenLike j, Parser t, IsString t) => Parser t j
-- parseJsonTokenNull = string "null" >> return jsonTokenNull

-- parseJsonTokenBoolean :: (JsonTokenLike j, Parser t, IsString t) => Parser t j
-- parseJsonTokenBoolean = true <|> false
--   where
--     true  = string "true"   >> return (jsonTokenBoolean True)
--     false = string "false"  >> return (jsonTokenBoolean False)

-- parseJsonTokenDouble :: (JsonTokenLike j, Parser t, IsString t) => Parser t j
-- parseJsonTokenDouble = liftM jsonTokenNumber rational

-- parseJsonToken :: (JsonTokenLike j, Parser t, IsString t) => Parser t j
-- parseJsonToken =
--   parseJsonTokenString     <|>
--   parseJsonTokenBraceL     <|>
--   parseJsonTokenBraceR     <|>
--   parseJsonTokenBracketL   <|>
--   parseJsonTokenBracketR   <|>
--   parseJsonTokenComma      <|>
--   parseJsonTokenColon      <|>
--   parseJsonTokenWhitespace <|>
--   parseJsonTokenNull       <|>
--   parseJsonTokenBoolean    <|>
--   parseJsonTokenDouble
