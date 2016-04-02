{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module HaskellWorks.Data.Conduit.Json
  ( blankedJsonToInterestBits
  , byteStringToBits
  , markerToByteString
  , blankedJsonToBalancedParens
  , jsonToken2Markers
  , textToJsonToken
  , interestingWord8s
  , jsonToken2BalancedParens
  ) where

import           Control.Monad
import           Control.Monad.Trans.Resource                         (MonadThrow)
import           Data.Array.Unboxed                                   as A
import qualified Data.Bits                                            as BITS
import           Data.ByteString                                      as BS
import           Data.Conduit
import           Data.Int
import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Conduit.Json.Words
import           HaskellWorks.Data.Conduit.Tokenize.Attoparsec
import           HaskellWorks.Data.Conduit.Tokenize.Attoparsec.Offset
import           HaskellWorks.Data.Json.Final.Tokenize
import           HaskellWorks.Data.Json.Token
import           Prelude                                              as P

interestingWord8s :: A.Array Word8 Word8
interestingWord8s = A.array (0, 255) [
  (w, if w == wOpenBracket || w == wOpenBrace || w == wOpenParen || w == wt || w == wf || w == wn || w == w1
    then 1
    else 0)
  | w <- [0 .. 255]]

blankedJsonToInterestBits :: Monad m => Conduit BS.ByteString m BS.ByteString
blankedJsonToInterestBits = blankedJsonToInterestBits' ""

padRight :: Word8 -> Int -> BS.ByteString -> BS.ByteString
padRight w n bs = if BS.length bs >= n then bs else fst (BS.unfoldrN n gen bs)
  where gen :: ByteString -> Maybe (Word8, ByteString)
        gen cs = case BS.uncons cs of
          Just (c, ds) -> Just (c, ds)
          Nothing      -> Just (w, BS.empty)

blankedJsonToInterestBits' :: Monad m => BS.ByteString -> Conduit BS.ByteString m BS.ByteString
blankedJsonToInterestBits' rs = do
  mbs <- await
  case mbs of
    Just bs -> do
      let cs = if BS.length rs /= 0 then BS.concat [rs, bs] else bs
      let lencs = BS.length cs
      let q = lencs + 7 `quot` 8
      let (ds, es) = BS.splitAt (q * 8) cs
      let (fs, _) = BS.unfoldrN q gen ds
      yield fs
      blankedJsonToInterestBits' es
    Nothing -> return ()
  where gen :: ByteString -> Maybe (Word8, ByteString)
        gen as = if BS.length as == 0
          then Nothing
          else Just ( BS.foldr (\b m -> (interestingWord8s ! b) .|. (m .<. 1)) 0 (padRight 0 8 (BS.take 8 as))
                    , BS.drop 8 as
                    )

markerToByteString' :: Monad m => Int64 -> Word8 -> Conduit Int64 m BS.ByteString
markerToByteString' a v = do
  mo <- await
  case mo of
    Just o -> if o < (a + 8)
      then markerToByteString' a (BITS.bit (fromIntegral (o - a)) .|. v)
      else do
        yield $ BS.singleton v
        leftover o
        markerToByteString' (a + 8) 0
    Nothing -> when (v /= 0) $ yield $ BS.singleton v

markerToByteString :: Monad m => Conduit Int64 m BS.ByteString
markerToByteString = markerToByteString' 0 0

textToJsonToken :: MonadThrow m => Conduit BS.ByteString m (ParseDelta Offset, JsonToken)
textToJsonToken = conduitParser (Offset 0) parseJsonToken

jsonToken2Markers :: Monad m => Conduit (ParseDelta Offset, JsonToken) m Int64
jsonToken2Markers = do
  mi <- await
  case mi of
    Just (ParseDelta (Offset start) _, token) -> do
      case token of
        JsonTokenBraceL     -> yield $ fromIntegral start
        JsonTokenBraceR     -> return ()
        JsonTokenBracketL   -> yield $ fromIntegral start
        JsonTokenBracketR   -> return ()
        JsonTokenComma      -> return ()
        JsonTokenColon      -> return ()
        JsonTokenWhitespace -> return ()
        JsonTokenString _   -> yield $ fromIntegral start
        JsonTokenBoolean _  -> yield $ fromIntegral start
        JsonTokenNumber _   -> yield $ fromIntegral start
        JsonTokenNull       -> yield $ fromIntegral start
      jsonToken2Markers
    Nothing -> return ()

jsonToken2BalancedParens :: Monad m => Conduit (ParseDelta Offset, JsonToken) m Bool
jsonToken2BalancedParens = do
  mi <- await
  case mi of
    Just (ParseDelta (Offset _) _, token) -> do
      case token of
        JsonTokenBraceL     -> yield True
        JsonTokenBraceR     -> yield False
        JsonTokenBracketL   -> yield True
        JsonTokenBracketR   -> yield False
        JsonTokenComma      -> return ()
        JsonTokenColon      -> return ()
        JsonTokenWhitespace -> return ()
        JsonTokenString _   -> yield True >> yield False
        JsonTokenBoolean _  -> yield True >> yield False
        JsonTokenNumber _   -> yield True >> yield False
        JsonTokenNull       -> yield True >> yield False
      jsonToken2BalancedParens
    Nothing -> return ()

blankedJsonToBalancedParens :: Monad m => Conduit BS.ByteString m Bool
blankedJsonToBalancedParens = do
  mbs <- await
  case mbs of
    Just bs -> blankedJsonToBalancedParens' bs
    Nothing -> return ()

blankedJsonToBalancedParens' :: Monad m => BS.ByteString -> Conduit BS.ByteString m Bool
blankedJsonToBalancedParens' bs = case BS.uncons bs of
  Just (c, cs) -> do
    case c of
      d | d == wOpenBrace     -> yield True
      d | d == wCloseBrace    -> yield False
      d | d == wOpenBracket   -> yield True
      d | d == wCloseBracket  -> yield False
      d | d == wOpenParen     -> yield True
      d | d == wCloseParen    -> yield False
      d | d == wt             -> yield True >> yield False
      d | d == wf             -> yield True >> yield False
      d | d == w1             -> yield True >> yield False
      d | d == wn             -> yield True >> yield False
      _                       -> return ()
    blankedJsonToBalancedParens' cs
  Nothing -> return ()

------------------------

yieldBitsOfWord8 :: Monad m => Word8 -> Conduit BS.ByteString m Bool
yieldBitsOfWord8 w = do
  yield ((w .&. BITS.bit 0) /= 0)
  yield ((w .&. BITS.bit 1) /= 0)
  yield ((w .&. BITS.bit 2) /= 0)
  yield ((w .&. BITS.bit 3) /= 0)
  yield ((w .&. BITS.bit 4) /= 0)
  yield ((w .&. BITS.bit 5) /= 0)
  yield ((w .&. BITS.bit 6) /= 0)
  yield ((w .&. BITS.bit 7) /= 0)

yieldBitsofWord8s :: Monad m => [Word8] -> Conduit BS.ByteString m Bool
yieldBitsofWord8s = P.foldr ((>>) . yieldBitsOfWord8) (return ())

byteStringToBits :: Monad m => Conduit BS.ByteString m Bool
byteStringToBits = do
  mbs <- await
  case mbs of
    Just bs -> yieldBitsofWord8s (BS.unpack bs) >> byteStringToBits
    Nothing -> return ()
