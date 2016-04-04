{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Conduit.Json.Blank
  ( blankJson
  ) where

import           Control.Monad
import           Control.Monad.Trans.Resource         (MonadThrow)
import           Data.ByteString                      as BS
import           Data.Conduit
import           Data.Word
import           HaskellWorks.Data.Conduit.Json.Words
import           Prelude                              as P

data BlankState
  = Escaped
  | InJson
  | InString
  | InNumber
  | InIdent

blankJson :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
blankJson = blankJson' InJson

blankJson' :: MonadThrow m => BlankState -> Conduit BS.ByteString m BS.ByteString
blankJson' lastState = do
  mbs <- await
  case mbs of
    Just bs -> do
      let (!cs, Just (!nextState, _)) = unfoldrN (BS.length bs) blankByteString (lastState, bs)
      yield cs
      blankJson' nextState
    Nothing -> return ()
  where
    blankByteString :: (BlankState, ByteString) -> Maybe (Word8, (BlankState, ByteString))
    blankByteString (InJson, bs) = case BS.uncons bs of
      Just (!c, !cs) | isLeadingDigit c   -> Just (w1         , (InNumber , cs))
      Just (!c, !cs) | c == wDoubleQuote  -> Just (wOpenParen , (InString , cs))
      Just (!c, !cs) | isAlphabetic c     -> Just (c          , (InIdent  , cs))
      Just (!c, !cs)                      -> Just (c          , (InJson   , cs))
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
      Just (!c, !cs) | isTrailingDigit c  -> Just (w0         , (InNumber , cs))
      Just (!c, !cs) | c == wDoubleQuote  -> Just (wOpenParen , (InString , cs))
      Just (!c, !cs) | isAlphabetic c     -> Just (c          , (InIdent  , cs))
      Just (!c, !cs)                      -> Just (c          , (InJson   , cs))
      Nothing                             -> Nothing
    blankByteString (InIdent, bs) = case BS.uncons bs of
      Just (!c, !cs) | isAlphabetic c     -> Just (wUnderscore, (InIdent  , cs))
      Just (!c, !cs) | isLeadingDigit c   -> Just (w1         , (InNumber , cs))
      Just (!c, !cs) | c == wDoubleQuote  -> Just (wOpenParen , (InString , cs))
      Just (!c, !cs)                      -> Just (c          , (InJson   , cs))
      Nothing                             -> Nothing
