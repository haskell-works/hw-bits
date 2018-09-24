{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module HaskellWorks.Data.Bits.Bits
  ( pattern Empty

  , defaultChunkBytes
  , defaultChunkWord64s
  ) where

import Data.Word
import HaskellWorks.Data.AtIndex          ((!!!))
import HaskellWorks.Data.Bits.BitPatterns
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning

import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.ByteString.Lazy.Internal       as LBSI
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.AtIndex           as HW
import qualified HaskellWorks.Data.ByteString        as BS
import qualified HaskellWorks.Data.ByteString.Lazy   as LBS
import qualified HaskellWorks.Data.Vector.AsVector64 as DVS

pattern (:~) :: BS.ByteString -> LBS.ByteString -> LBS.ByteString
pattern (:~) bs bss = LBSI.Chunk bs bss

pattern Nil :: LBS.ByteString
pattern Nil = LBSI.Empty

{-# COMPLETE Nil, (:~) #-}

infixr 6 :~

pattern Empty :: BitString
pattern Empty = BitString LBSI.Empty

defaultChunkBytes :: Int
defaultChunkBytes = (LBSI.defaultChunkSize `div` 64) * 64
{-# INLINE defaultChunkBytes #-}

defaultChunkWord64s :: Int
defaultChunkWord64s = defaultChunkBytes `div` 8
{-# INLINE defaultChunkWord64s #-}

newtype BitString = BitString
  { bits :: LBS.ByteString
  } deriving (Eq, Show)

class ToBitString a where
  toBitString :: a -> BitString

instance ToBitString BitString where
  toBitString = id
  {-# INLINE toBitString #-}

instance ToBitString [BS.ByteString] where
  toBitString = BitString . LBS.fromChunks
  {-# INLINE toBitString #-}

instance ToBitString BS.ByteString where
  toBitString = BitString . LBS.fromStrict
  {-# INLINE toBitString #-}

instance ToBitString LBS.ByteString where
  toBitString = BitString
  {-# INLINE toBitString #-}

instance ToBitString (DVS.Vector Word8) where
  toBitString = toBitString . BS.toByteString
  {-# INLINE toBitString #-}

instance ToBitString (DVS.Vector Word64) where
  toBitString = toBitString . BS.toByteString
  {-# INLINE toBitString #-}

instance ToBitString [DVS.Vector Word8] where
  toBitString = toBitString . fmap BS.toByteString
  {-# INLINE toBitString #-}

instance ToBitString [DVS.Vector Word64] where
  toBitString = toBitString . fmap BS.toByteString
  {-# INLINE toBitString #-}

instance BS.ToByteStrings BitString where
  toByteStrings (BitString bs) = LBS.toChunks bs

instance BitPatterns BitString where
  all0s :: BitString
  all0s = BitString s
    where s = LBSI.Chunk chunkOf0s s
  {-# INLINE all0s #-}

  all1s :: BitString
  all1s = BitString s
    where s = LBSI.Chunk chunkOf1s s
  {-# INLINE all1s #-}

chunkOf0s :: BS.ByteString
chunkOf0s = BS.replicate defaultChunkBytes 0x00
{-# NOINLINE chunkOf0s #-}

chunkOf1s :: BS.ByteString
chunkOf1s = BS.replicate defaultChunkBytes 0xff
{-# NOINLINE chunkOf1s #-}

instance Shift BitString where
  BitString ass .<. n = if n >= 0 && n <= 64
    then BitString (go (LBS.resegment 8 ass))
    else error $ "Invalid shift" <> show n
    where go :: LBS.ByteString -> LBS.ByteString
          go bss = case bss of
            cs:~css@(ds:~_) -> buildChunk cs (DVS.head (DVS.asVector64 ds)):~go css
            cs:~Nil         -> buildChunk cs 0                             :~Nil
            Nil             -> Nil
          buildChunk :: BS.ByteString -> Word64 -> BS.ByteString
          buildChunk bs w = BS.take (BS.length bs) (BS.toByteString (buildChunk64 (DVS.asVector64 (chunkPaddedByteString bs)) w))
          buildChunk64 :: DVS.Vector Word64 -> Word64 -> DVS.Vector Word64
          buildChunk64 v w = DVS.constructN defaultChunkWord64s buildGo
            where vLastPos = fromIntegral (defaultChunkWord64s - 1) :: Position
                  buildGo :: DVS.Vector Word64 -> Word64
                  buildGo u = ((v !!! ui) .<. n) .|. if ui < vLastPos
                    then (v !!! (ui + 1)) .>. (64 - n)
                    else w                .>. (64 - n)
                    where ui = HW.end u
  {-# INLINE (.<.) #-}

  BitString ass .>. n = if n >= 0 && n <= 64
    then BitString (go 0 (LBS.resegment 8 ass))
    else error $ "Invalid shift" <> show n
    where go :: Word64 -> LBS.ByteString -> LBS.ByteString
          go w bss = case bss of
            cs:~css -> buildChunk w cs:~go (DVS.last (DVS.asVector64 (chunkPaddedByteString cs))) css
            Nil     -> Nil
          buildChunk :: Word64 -> BS.ByteString -> BS.ByteString
          buildChunk w bs = BS.take (BS.length bs) (BS.toByteString (buildChunk64 w (DVS.asVector64 (chunkPaddedByteString bs))))
          buildChunk64 :: Word64 -> DVS.Vector Word64 -> DVS.Vector Word64
          buildChunk64 w v = DVS.constructN defaultChunkWord64s buildGo
            where buildGo :: DVS.Vector Word64 -> Word64
                  buildGo u = ((v !!! ui) .>. n) .|. if ui > 0
                    then (v !!! (ui - 1)) .<. (64 - n)
                    else w                .<. (64 - n)
                    where ui = HW.end u
  {-# INLINE (.>.) #-}

chunkPaddedByteString :: BS.ByteString -> BS.ByteString
chunkPaddedByteString v = if BS.length v >= defaultChunkBytes
  then v
  else v <> BS.replicate ((defaultChunkBytes - BS.length v) `max` 0) 0
{-# INLINE chunkPaddedByteString #-}
