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

blankEscapedChars :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
blankEscapedChars = blankEscapedChars' ""

blankEscapedChars' :: MonadThrow m => BS.ByteString -> Conduit BS.ByteString m BS.ByteString
blankEscapedChars' rs = do
  mbs <- await
  case mbs of
    Just bs -> do
      let cs = if BS.length rs /= 0 then BS.concat [rs, bs] else bs
      let ds = fst (unfoldrN (BS.length cs) unescapeByteString (False, cs))
      yield ds
      blankEscapedChars' (BS.drop (BS.length ds) cs)
    Nothing -> when (BS.length rs > 0) (yield rs)
  where
    unescapeByteString :: (Bool, ByteString) -> Maybe (Word8, (Bool, ByteString))
    unescapeByteString (wasEscaped, bs) = case BS.uncons bs of
      Just (c, cs) -> if wasEscaped
        then Just (wUnderscore, (False, cs))
        else if c /= wBackslash
          then Just (c, (False, cs))
          else Just (c, (True, cs))
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
