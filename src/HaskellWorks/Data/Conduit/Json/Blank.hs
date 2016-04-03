{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Conduit.Json.Blank
  ( blankEscapedChars
  , blankIdentifiers
  , blankNumbers
  , blankStrings
  , blankJson
  ) where

import           Control.Monad
import           Control.Monad.Trans.Resource         (MonadThrow)
import           Data.ByteString                      as BS
import           Data.Conduit
import           Data.Word
import           HaskellWorks.Data.Conduit.Json.Words
import           Prelude                              as P

data FastState
  = NotEscaped
  | Escaped

blankEscapedChars :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
blankEscapedChars = blankEscapedChars' NotEscaped

blankEscapedChars' :: MonadThrow m => FastState -> Conduit BS.ByteString m BS.ByteString
blankEscapedChars' lastState = do
  !mbs <- await
  case mbs of
    Just bs -> do
      let (!cs, Just (!nextState, _)) = unfoldrN (BS.length bs) unescapeByteString (lastState, bs)
      yield cs
      blankEscapedChars' nextState
    Nothing -> return ()
  where
    unescapeByteString :: (FastState, ByteString) -> Maybe (Word8, (FastState, ByteString))
    unescapeByteString (Escaped, bs) = case BS.uncons bs of
      Just (_, !cs) -> Just (wUnderscore, (NotEscaped, cs))
      Nothing       -> Nothing
    unescapeByteString (NotEscaped, bs) = case BS.uncons bs of
      Just (!c, !cs) | c /= wBackslash  -> Just (c, (NotEscaped, cs))
      Just (!c, !cs)                    -> Just (c, (Escaped   , cs))
      Nothing                           -> Nothing

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

blankIdentifiers :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
blankIdentifiers = blankIdentifiers' False

blankIdentifiers' :: MonadThrow m => Bool -> Conduit BS.ByteString m BS.ByteString
blankIdentifiers' wasIdentifier = do
  mbs <- await
  case mbs of
    Just bs -> case unfoldrN (BS.length bs) blankByteString (wasIdentifier, bs) of
      (cs, Just (nextInIdentifier, _)) -> do
        yield cs
        blankStrings' nextInIdentifier
      (cs, _) -> yield cs
    Nothing -> return ()
  where
    blankByteString :: (Bool, ByteString) -> Maybe (Word8, (Bool, ByteString))
    blankByteString (isInIdentifier, bs) = case BS.uncons bs of
      Just (c, cs) | isInIdentifier && isAlphabetic c -> Just (wUnderscore, (True  , cs))
      Just (c, cs) | isAlphabetic c                   -> Just (c          , (True  , cs))
      Just (c, cs)                                    -> Just (c          , (False , cs))
      Nothing                                         -> Nothing

blankJson :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
blankJson = blankEscapedChars =$= blankStrings =$= blankNumbers =$= blankIdentifiers
