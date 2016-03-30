{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Conduit.Json.Blank
  ( blankEscapedChars
  , blankNumbers
  , blankStrings
  ) where

import           Control.Monad
import           Control.Monad.Trans.Resource (MonadThrow)
import           Data.ByteString              as BS
import           Data.Char
import           Data.Conduit
import           Data.Word
import           Prelude                      as P

wBackslash :: Word8
wBackslash = 92

wDoubleQuote :: Word8
wDoubleQuote = 34

wUnderscore :: Word8
wUnderscore = 95

wSpace :: Word8
wSpace = 32

wOpenParen :: Word8
wOpenParen = 40

wCloseParen :: Word8
wCloseParen = 41

wPlus :: Word8
wPlus = fromIntegral (ord '+')

we :: Word8
we = fromIntegral (ord 'e')

wE :: Word8
wE = fromIntegral (ord 'E')

wDot :: Word8
wDot = fromIntegral (ord '.')

wMinus :: Word8
wMinus = fromIntegral (ord '-')

w0 :: Word8
w0 = fromIntegral (ord '0')

w1 :: Word8
w1 = fromIntegral (ord '1')

w9 :: Word8
w9 = fromIntegral (ord '9')

blankEscapedChars :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
blankEscapedChars = blankEscapedChars' ""

blankEscapedChars' :: MonadThrow m => BS.ByteString -> Conduit BS.ByteString m BS.ByteString
blankEscapedChars' rs = do
  mbs <- await
  case mbs of
    Just bs -> do
      let cs = BS.concat [rs, bs]
      let ds = fst (unfoldrN (BS.length cs) unescapeByteString cs)
      yield ds
      blankEscapedChars' (BS.drop (BS.length ds) cs)
    Nothing -> when (BS.length rs > 0) (yield rs)
  where
    unescapeByteString bs = case BS.uncons bs of
      Just (c, cs) -> case BS.uncons cs of
        Just (_, ds) -> if c /= wBackslash
          then Just (c, cs)
          else Just (c, BS.cons wUnderscore ds)
        Nothing -> if c /= wBackslash
          then Just (c, cs)
          else Nothing
      Nothing -> Nothing

blankStrings :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
blankStrings = blankStrings' False

blankStrings' :: MonadThrow m => Bool -> Conduit BS.ByteString m BS.ByteString
blankStrings' wasInString = do
  mbs <- await
  case mbs of
    Just bs -> case unfoldrN (BS.length bs) blankByteString (wasInString, bs) of
      (cs, Just (nextInString, _)) -> do
        yield cs
        blankStrings' nextInString
      (cs, _) -> yield cs
    Nothing -> return ()
  where
    blankByteString :: (Bool, ByteString) -> Maybe (Word8, (Bool, ByteString))
    blankByteString (isInString, bs) = case BS.uncons bs of
      Just (c, cs) -> if c /= wDoubleQuote
        then Just (if isInString then wSpace      else c          , (isInString     , cs))
        else Just (if isInString then wCloseParen else wOpenParen , (not isInString , cs))
      Nothing -> Nothing

isLeadingDigit :: Word8 -> Bool
isLeadingDigit w = w == wMinus || (w >= w0 && w <= w9)

isTrailingDigit :: Word8 -> Bool
isTrailingDigit w = w == wPlus || w == wMinus || (w >= w0 && w <= w9) || w == wDot || w == wE || w == we

blankNumbers :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
blankNumbers = blankNumbers' False

blankNumbers' :: MonadThrow m => Bool -> Conduit BS.ByteString m BS.ByteString
blankNumbers' wasInNumber = do
  mbs <- await
  case mbs of
    Just bs -> case unfoldrN (BS.length bs) blankByteString (wasInNumber, bs) of
      (cs, Just (nextInNumber, _)) -> do
        yield cs
        blankStrings' nextInNumber
      (cs, _) -> yield cs
    Nothing -> return ()
  where
    blankByteString :: (Bool, ByteString) -> Maybe (Word8, (Bool, ByteString))
    blankByteString (isInNumber, bs) = case BS.uncons bs of
      Just (c, cs) | isInNumber && isTrailingDigit c  -> Just (w0, (True  , cs))
      Just (c, cs) | isLeadingDigit c                 -> Just (w1, (True  , cs))
      Just (c, cs)                                    -> Just (c,  (False , cs))
      Nothing                                         -> Nothing
