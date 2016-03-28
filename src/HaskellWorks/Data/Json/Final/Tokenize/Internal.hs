{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Final.Tokenize.Internal
    ( IsChar(..)
    , JsonToken(..)
    , AFP.Parser(..)
    , parseJsonToken
    , parseJsonTokenString
    , escapedChar
    ) where

import           Control.Applicative
import           Control.Monad
import qualified Data.Attoparsec.ByteString.Char8          as BC
import qualified Data.Attoparsec.Combinator                as AC
import qualified Data.Attoparsec.Types                     as T
import           Data.Bits
import           Data.Char
import           Data.String
import           HaskellWorks.Data.Attoparsec.Final.IsChar
import           HaskellWorks.Data.Attoparsec.Final.Parser as AFP
import           HaskellWorks.Data.Json.Token

hexDigitNumeric :: AFP.Parser t => T.Parser t Int
hexDigitNumeric = do
  c <- satisfyChar (\c -> '0' <= c && c <= '9')
  return $ ord c - ord '0'

hexDigitAlphaLower :: AFP.Parser t => T.Parser t Int
hexDigitAlphaLower = do
  c <- satisfyChar (\c -> 'a' <= c && c <= 'z')
  return $ ord c - ord 'a' + 10

hexDigitAlphaUpper :: AFP.Parser t => T.Parser t Int
hexDigitAlphaUpper = do
  c <- satisfyChar (\c -> 'A' <= c && c <= 'Z')
  return $ ord c - ord 'A' + 10

hexDigit :: AFP.Parser t => T.Parser t Int
hexDigit = hexDigitNumeric <|> hexDigitAlphaLower <|> hexDigitAlphaUpper

verbatimChar :: AFP.Parser t => T.Parser t Char
verbatimChar  = satisfyChar (BC.notInClass "\"\\") <?> "invalid string character"

escapedChar :: (IsString t, AFP.Parser t) => T.Parser t Char
escapedChar   = do
  _ <- string "\\"
  (   char '"'  >> return '"'  ) <|>
    ( char 'b'  >> return '\b' ) <|>
    ( char 'n'  >> return '\n' ) <|>
    ( char 'f'  >> return '\f' ) <|>
    ( char 'r'  >> return '\r' ) <|>
    ( char 't'  >> return '\t' ) <|>
    ( char '\\' >> return '\\' ) <|>
    ( char '\'' >> return '\'' ) <|>
    ( char '/'  >> return '/'  )

escapedCode :: (IsString t, AFP.Parser t) => T.Parser t Char
escapedCode   = do
  _ <- string "\\u"
  a <- hexDigit
  b <- hexDigit
  c <- hexDigit
  d <- hexDigit
  return $ chr $ a `shift` 24 .|. b `shift` 16 .|. c `shift` 8 .|. d

parseJsonTokenString :: (JsonTokenLike j, AFP.Parser t, Alternative (T.Parser t), IsString t) => T.Parser t j
parseJsonTokenString = do
  _ <- string "\""
  value <- many (verbatimChar <|> escapedChar <|> escapedCode)
  _ <- string "\""
  return $ jsonTokenString value

parseJsonTokenBraceL :: (JsonTokenLike j, AFP.Parser t, IsString t) => T.Parser t j
parseJsonTokenBraceL = string "{" >> return jsonTokenBraceL

parseJsonTokenBraceR :: (JsonTokenLike j, AFP.Parser t, IsString t) => T.Parser t j
parseJsonTokenBraceR = string "}" >> return jsonTokenBraceR

parseJsonTokenBracketL :: (JsonTokenLike j, AFP.Parser t, IsString t) => T.Parser t j
parseJsonTokenBracketL = string "[" >> return jsonTokenBracketL

parseJsonTokenBracketR :: (JsonTokenLike j, AFP.Parser t, IsString t) => T.Parser t j
parseJsonTokenBracketR = string "]" >> return jsonTokenBracketR

parseJsonTokenComma :: (JsonTokenLike j, AFP.Parser t, IsString t) => T.Parser t j
parseJsonTokenComma = string "," >> return jsonTokenComma

parseJsonTokenColon :: (JsonTokenLike j, AFP.Parser t, IsString t) => T.Parser t j
parseJsonTokenColon = string ":" >> return jsonTokenColon

parseJsonTokenWhitespace :: (JsonTokenLike j, AFP.Parser t, IsString t) => T.Parser t j
parseJsonTokenWhitespace = do
  _ <- AC.many1' $ BC.choice [string " ", string "\t", string "\n", string "\r"]
  return jsonTokenWhitespace

parseJsonTokenNull :: (JsonTokenLike j, AFP.Parser t, IsString t) => T.Parser t j
parseJsonTokenNull = string "null" >> return jsonTokenNull

parseJsonTokenBoolean :: (JsonTokenLike j, AFP.Parser t, IsString t) => T.Parser t j
parseJsonTokenBoolean = true <|> false
  where
    true  = string "true"   >> return (jsonTokenBoolean True)
    false = string "false"  >> return (jsonTokenBoolean False)

parseJsonTokenDouble :: (JsonTokenLike j, AFP.Parser t, IsString t) => T.Parser t j
parseJsonTokenDouble = liftM jsonTokenNumber rational

parseJsonToken :: (JsonTokenLike j, AFP.Parser t, IsString t) => T.Parser t j
parseJsonToken =
  parseJsonTokenString     <|>
  parseJsonTokenBraceL     <|>
  parseJsonTokenBraceR     <|>
  parseJsonTokenBracketL   <|>
  parseJsonTokenBracketR   <|>
  parseJsonTokenComma      <|>
  parseJsonTokenColon      <|>
  parseJsonTokenWhitespace <|>
  parseJsonTokenNull       <|>
  parseJsonTokenBoolean    <|>
  parseJsonTokenDouble
