{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

-- TODO: Create Parsec parser for Json Tokens
module HaskellWorks.Data.Json.Tokenize.Parsec where

import qualified Data.Vector.Storable as DVS
import           Data.Word8
import           Text.Parsec

type VW8Parser a = ParsecT (DVS.Vector Word8) () IO a

instance (Monad m, DVS.Storable a) => Stream (DVS.Vector a) m a where
  uncons v | DVS.null v = return Nothing
           | otherwise = return (Just (DVS.head v, DVS.tail v))

isHexDigitChar :: Char -> Bool
isHexDigitChar c =
  '0' <= c && c <= '9' ||
  'a' <= c && c <= 'z' ||
  'A' <= c && c <= 'Z'

mchar :: Char -> VW8Parser Integer
mchar _ = undefined

pmain :: VW8Parser Integer
pmain = undefined

satisfyChar :: VW8Parser Char
satisfyChar = tokenPrim
  (const "moo")
  (\pos _ _ -> pos)
  (\_ -> Just ' ')
