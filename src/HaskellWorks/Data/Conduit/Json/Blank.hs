{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Conduit.Json.Blank
  ( blankIdentifiers
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
  = Escaped
  | InJson
  | InString
  | InNumber

blankStrings :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
blankStrings = blankStrings' InJson

blankStrings' :: MonadThrow m => FastState -> Conduit BS.ByteString m BS.ByteString
blankStrings' lastState = do
  mbs <- await
  case mbs of
    Just bs -> do
      let (!cs, Just (!nextState, _)) = unfoldrN (BS.length bs) blankByteString (lastState, bs)
      yield cs
      blankStrings' nextState
    Nothing -> return ()
  where
    blankByteString :: (FastState, ByteString) -> Maybe (Word8, (FastState, ByteString))
    blankByteString (InJson, bs) = case BS.uncons bs of
      Just (!c, !cs) | isLeadingDigit c   -> Just (w1         , (InNumber , cs))
      Just (!c, !cs) | c /= wDoubleQuote  -> Just (c          , (InJson   , cs))
      Just ( _, !cs)                      -> Just (wOpenParen , (InString , cs))
      Nothing -> Nothing
    blankByteString (InString, bs) = case BS.uncons bs of
      Just (!c, !cs) | c == wBackslash    -> Just (wSpace     , (Escaped  , cs))
      Just (!c, !cs) | c == wDoubleQuote  -> Just (wCloseParen, (InJson   , cs))
      Just (_ , !cs)                      -> Just (wSpace     , (InString , cs))
      Nothing                             -> Nothing
    blankByteString (Escaped, bs) = case BS.uncons bs of
      Just (_, !cs)                       -> Just (wSpace, (InString, cs))
      Nothing                             -> Nothing
    blankByteString (InNumber, bs) = case BS.uncons bs of
      Just (!c, !cs) | isTrailingDigit c  -> Just (w0, (InNumber, cs))
      Just (!c, !cs)                      -> Just (c,  (InJson  , cs))
      Nothing                             -> Nothing

blankIdentifiers :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
blankIdentifiers = blankIdentifiers' False

blankIdentifiers' :: MonadThrow m => Bool -> Conduit BS.ByteString m BS.ByteString
blankIdentifiers' wasIdentifier = do
  mbs <- await
  case mbs of
    Just bs -> case unfoldrN (BS.length bs) blankByteString (wasIdentifier, bs) of
      (cs, Just (nextInIdentifier, _)) -> do
        yield cs
        blankIdentifiers' nextInIdentifier
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
blankJson = blankStrings =$= blankIdentifiers
