{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module HaskellWorks.Data.Bits.BitString
  ( BitString(..)
  , ToBitString(..)
  , defaultChunkBytes
  , defaultChunkWord64s
  , lengthBytes
  , takeBytes
  ) where

import Data.Semigroup                     ((<>))
import Data.Word
import HaskellWorks.Data.AtIndex          ((!!!))
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Bits.BitPatterns
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Positioning

import qualified Data.ByteString                     as BS
import qualified Data.ByteString.Builder             as BSB
import qualified Data.ByteString.Internal            as BSI
import qualified Data.ByteString.Lazy                as LBS
import qualified Data.Vector.Storable                as DVS
import qualified Data.Vector.Storable.Mutable        as DVSM
import qualified Foreign.Ptr                         as F
import qualified HaskellWorks.Data.AtIndex           as HW
import qualified HaskellWorks.Data.ByteString        as BS
import qualified HaskellWorks.Data.Take              as HW
import qualified HaskellWorks.Data.Vector.AsVector64 as DVS
import qualified System.IO.Unsafe                    as IO

defaultChunkBytes :: Int
defaultChunkBytes = 512
{-# INLINE defaultChunkBytes #-}

defaultChunkWord64s :: Int
defaultChunkWord64s = defaultChunkBytes `div` 8
{-# INLINE defaultChunkWord64s #-}

newtype BitString = BitString
  { bits :: [BS.ByteString]
  } deriving (Eq, Show)

class ToBitString a where
  toBitString :: a -> BitString

instance ToBitString BitString where
  toBitString = id
  {-# INLINE toBitString #-}

instance ToBitString [BS.ByteString] where
  toBitString = BitString . BS.rechunk defaultChunkBytes
  {-# INLINE toBitString #-}

instance ToBitString BS.ByteString where
  toBitString = toBitString . (:[])
  {-# INLINE toBitString #-}

instance ToBitString LBS.ByteString where
  toBitString = toBitString . LBS.toChunks
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
  toByteStrings (BitString bs) = bs

instance BitWise BitString where
  (.&.) :: BitString -> BitString -> BitString
  BitString as .&. BitString bs = BitString (uncurry (.&.) <$> zip as bs)
  {-# INLINE (.&.) #-}

  (.|.) :: BitString -> BitString -> BitString
  BitString as .|. BitString bs = BitString (uncurry (.|.) <$> zip as bs)
  {-# INLINE (.|.) #-}

  (.^.) :: BitString -> BitString -> BitString
  BitString as .^. BitString bs = BitString (uncurry (.^.) <$> zip as bs)
  {-# INLINE (.^.) #-}

  comp  :: BitString -> BitString
  comp (BitString as) = BitString (comp <$> as)
  {-# INLINE comp #-}

instance BitPatterns BitString where
  all0s :: BitString
  all0s = BitString (repeat chunkOf0s)
  {-# INLINE all0s #-}

  all1s :: BitString
  all1s = BitString (repeat chunkOf1s)
  {-# INLINE all1s #-}

instance Shift BitString where
  BitString ass .<. n = if n >= 0 && n <= 64
    then BitString (go ass)
    else error $ "Invalid shift" <> show n
    where go :: [BS.ByteString] -> [BS.ByteString]
          go bss = case bss of
            (cs:css@(ds:_)) -> buildChunk cs (DVS.head (DVS.asVector64 ds)):go css
            [cs]            -> buildChunk cs 0                             :[]
            []              -> []
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
    then BitString (go 0 ass)
    else error $ "Invalid shift" <> show n
    where go :: Word64 -> [BS.ByteString] -> [BS.ByteString]
          go w bss = case bss of
            (cs:css) -> buildChunk w cs:go (DVS.last (DVS.asVector64 (chunkPaddedByteString cs))) css
            []       -> []
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

instance BitLength BitString where
  bitLength bs = lengthBytes bs * 8
  {-# INLINE bitLength #-}

instance BitShow BitString where
  bitShows (BitString bss) = mconcat (bitShows <$> bss)
  {-# INLINE bitShows #-}

instance BitRead BitString where
  bitRead s = toBitString <$> (bitRead s :: Maybe BS.ByteString)
  {-# INLINE bitRead #-}

chunkOf0s :: BS.ByteString
chunkOf0s = BS.replicate defaultChunkBytes 0x00
{-# NOINLINE chunkOf0s #-}

chunkOf1s :: BS.ByteString
chunkOf1s = BS.replicate defaultChunkBytes 0xff
{-# NOINLINE chunkOf1s #-}

chunkPaddedByteString :: BS.ByteString -> BS.ByteString
chunkPaddedByteString v = if BS.length v >= defaultChunkBytes
  then v
  else v <> BS.replicate ((defaultChunkBytes - BS.length v) `max` 0) 0
{-# INLINE chunkPaddedByteString #-}

lengthBytes :: BitString -> Count
lengthBytes (BitString bss) = fromIntegral (sum (BS.length <$> bss))
{-# INLINE lengthBytes #-}

takeBytes :: Count -> BitString -> BitString
takeBytes n (BitString bss) = BitString (go n bss)
  where go :: Count -> [BS.ByteString] -> [BS.ByteString]
        go 0 _        = []
        go i (cs:css) = if i < HW.length cs then [HW.take i cs] else cs:go (i - HW.length cs) css
        go _ []       = []

edgesFrom0 :: [Count] -> [BS.ByteString]
edgesFrom0 [] = repeat chunkOf0s
edgesFrom0 cs = bs:ds
  where (bs, ds) = IO.unsafePerformIO $ BSI.createAndTrim' defaultChunkBytes (edgesFrom0' defaultChunkBytes cs)

edgesFrom0' :: Int -> [Count] -> F.Ptr Word8 -> IO (Int, Int, [BS.ByteString])
edgesFrom0' unwritten [] p = do
  BSI.memset p 0 (fromIntegral unwritten)
  return (0, defaultChunkBytes, repeat chunkOf0s)

edgesFrom1 :: [Count] -> [BS.ByteString]
edgesFrom1 [] = repeat chunkOf0s
edgesFrom1 cs = bs:ds
  where (bs, ds) = IO.unsafePerformIO $ BSI.createAndTrim' defaultChunkBytes (edgesFrom1' cs)

edgesFrom1' :: [Count] -> F.Ptr Word8 -> IO (Int, Int, [BS.ByteString])
edgesFrom1' [] = undefined
