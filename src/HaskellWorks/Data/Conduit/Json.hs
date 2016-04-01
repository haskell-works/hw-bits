{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes        #-}

module HaskellWorks.Data.Conduit.Json where

import           Control.Monad
import           Control.Monad.Trans.Resource                         (MonadThrow)
import qualified Data.Bits                                            as BITS
import           Data.ByteString                                      as BS
import           Data.ByteString.Internal                             as BS
import           Data.Conduit
import           Data.Int
import           Data.Word
import           Foreign.Ptr                                          (plusPtr)
import           Foreign.Storable                                     (Storable (..))
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Conduit.Json.Words
import           HaskellWorks.Data.Conduit.Tokenize.Attoparsec
import           HaskellWorks.Data.Conduit.Tokenize.Attoparsec.Offset
import           HaskellWorks.Data.Json.Final.Tokenize
import           HaskellWorks.Data.Json.Token
import           Prelude                                              as P

blankedJsonToInterestBits :: Monad m => Conduit BS.ByteString m BS.ByteString
blankedJsonToInterestBits = blankedJsonToInterestBits' ""

padRight :: Word8 -> Int -> BS.ByteString -> BS.ByteString
padRight w n bs = fst (BS.unfoldrN n gen bs)
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
          else Just ( BS.foldr (\b m -> interesting b .|. (m .<. 1)) 0 (padRight 0 8 (BS.take 8 as))
                    , BS.drop 8 as
                    )
        interesting :: Word8 -> Word8
        interesting w = if w == wOpenBracket || w == wOpenBrace || w == wOpenParen || w == wt || w == wf || w == wn || w == w1
          then 1
          else 0

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

unescape' :: MonadThrow m => BS.ByteString -> Conduit BS.ByteString m BS.ByteString
unescape' rs = do
  mbs <- await
  case mbs of
    Just bs -> do
      let cs = BS.concat [rs, bs]
      let ds = fst (unfoldrN (BS.length cs) unescapeByteString cs)
      yield ds
      unescape' (BS.drop (BS.length ds) cs)
    Nothing -> return ()

unescapeByteString :: ByteString -> Maybe (Word8, ByteString)
unescapeByteString bs = case BS.uncons bs of
  Just (c, cs) -> case BS.uncons cs of
    Just (_, ds) -> if c /= wBackslash
      then Just (c, cs)
      else Just (c, BS.cons wUnderscore ds)
    Nothing -> if c /= wBackslash
      then Just (c, cs)
      else Nothing
  Nothing -> Nothing

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

------------------------

bitsToByteString :: Monad m => Int64 -> Word8 -> Conduit Bool m BS.ByteString
bitsToByteString n w = do
  mb <- await
  case mb of
    Just b -> do
      let v = if b then w .|. BITS.bit (fromIntegral (n `mod` 8)) else w
      when (n `mod` 8 == 7) (yield $ BS.singleton v)
      bitsToByteString (n + 1) v
    Nothing -> when (w /= 0) (yield $ BS.singleton w)

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

mcons :: Maybe a -> [a] -> [a]
mcons (Just a) = (a:)
mcons Nothing = id

awaitWord8Or :: Monad m => Word8 -> Consumer ByteString m Word8
awaitWord8Or d = do
  mbs <- await
  case mbs of
    Just bs -> if BS.null bs
      then do
        leftover $ BS.tail bs
        return   $ BS.head bs
      else return d
    Nothing -> return d

unfinished :: Monad m => Consumer i m Bool
unfinished = do
  mi <- await
  case mi of
    Just i  -> leftover i >> return True
    Nothing -> return False

ifM :: Monad m => m Bool -> m a -> m a -> m a
ifM p a b = do
  t <- p
  if t then a else b

whenM :: Monad m => m Bool -> m () -> m ()
whenM p a = ifM p a (return ())

packW64 :: Word64 -> ByteString
packW64 w64 =
    unsafeCreate 8 $ \p -> do
      poke  p               (fromIntegral ( w64                      .|. 0xff) :: Word8)
      poke (p `plusPtr` 1)  (fromIntegral ((w64 `BITS.shift` (-8 ))  .|. 0xff) :: Word8)
      poke (p `plusPtr` 2)  (fromIntegral ((w64 `BITS.shift` (-16))  .|. 0xff) :: Word8)
      poke (p `plusPtr` 3)  (fromIntegral ((w64 `BITS.shift` (-24))  .|. 0xff) :: Word8)
      poke (p `plusPtr` 4)  (fromIntegral ((w64 `BITS.shift` (-32))  .|. 0xff) :: Word8)
      poke (p `plusPtr` 5)  (fromIntegral ((w64 `BITS.shift` (-40))  .|. 0xff) :: Word8)
      poke (p `plusPtr` 6)  (fromIntegral ((w64 `BITS.shift` (-48))  .|. 0xff) :: Word8)
      poke (p `plusPtr` 7)  (fromIntegral ((w64 `BITS.shift` (-56))  .|. 0xff) :: Word8)

bsToRank9BS :: Monad m => Word64 -> Conduit BS.ByteString m BS.ByteString
bsToRank9BS rank = whenM unfinished $ do
  v0 <- awaitWord8Or 0
  v1 <- awaitWord8Or 0
  v2 <- awaitWord8Or 0
  v3 <- awaitWord8Or 0
  v4 <- awaitWord8Or 0
  v5 <- awaitWord8Or 0
  v6 <- awaitWord8Or 0
  v7 <- awaitWord8Or 0
  let p0 =       fromIntegral (BITS.popCount v0)                  :: Word64
  let p1 = p0 + (fromIntegral (BITS.popCount v1) `BITS.shift` 9 ) :: Word64
  let p2 = p1 + (fromIntegral (BITS.popCount v2) `BITS.shift` 18) :: Word64
  let p3 = p2 + (fromIntegral (BITS.popCount v3) `BITS.shift` 27) :: Word64
  let p4 = p3 + (fromIntegral (BITS.popCount v4) `BITS.shift` 36) :: Word64
  let p5 = p4 + (fromIntegral (BITS.popCount v5) `BITS.shift` 45) :: Word64
  let p6 = p5 + (fromIntegral (BITS.popCount v6) `BITS.shift` 54) :: Word64
  let p7 = p6 + (fromIntegral (BITS.popCount v7) `BITS.shift` 54) :: Word64
  let newRank = rank + p7
  yield $ packW64 (p0 .|. p1 .|. p2 .|. p3 .|. p4 .|. p5 .|. p6)
  yield $ packW64 newRank
  bsToRank9BS newRank
