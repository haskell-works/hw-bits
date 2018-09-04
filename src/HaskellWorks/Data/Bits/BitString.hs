{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs      #-}

module HaskellWorks.Data.Bits.BitString
  ( BitString(..)
  , ToBitString(..)
  , defaultChunkBytes
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitPatterns
import HaskellWorks.Data.Bits.BitWise

import qualified Data.ByteString              as BS
import qualified Data.ByteString.Lazy         as LBS
import qualified Data.Vector.Storable         as DVS
import qualified HaskellWorks.Data.ByteString as BS

defaultChunkBytes :: Int
defaultChunkBytes = 512

newtype BitString = BitString
  { bits :: [BS.ByteString]
  } deriving (Eq)

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

instance BitWise BitString where
  (.&.) :: BitString -> BitString -> BitString
  BitString as .&. BitString bs = BitString (uncurry (.&.) <$> zip as bs)

  (.|.) :: BitString -> BitString -> BitString
  BitString as .|. BitString bs = BitString (uncurry (.|.) <$> zip as bs)

  (.^.) :: BitString -> BitString -> BitString
  BitString as .^. BitString bs = BitString (uncurry (.^.) <$> zip as bs)

  comp  :: BitString -> BitString
  comp (BitString as) = BitString (comp <$> as)

instance BitPatterns BitString where
  all0s :: BitString
  all0s = BitString (repeat chunkOf0s)
  {-# INLINE all0s #-}

  all1s :: BitString
  all1s = BitString (repeat chunkOf1s)
  {-# INLINE all1s #-}

chunkOf0s :: BS.ByteString
chunkOf0s = BS.replicate defaultChunkBytes 0x00
{-# NOINLINE chunkOf0s #-}

chunkOf1s :: BS.ByteString
chunkOf1s = BS.replicate defaultChunkBytes 0xff
{-# NOINLINE chunkOf1s #-}
