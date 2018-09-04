{-# LANGUAGE InstanceSigs #-}

module HaskellWorks.Data.Bits.BitString
  ( BitString(..)
  , defaultChunkSize
  ) where

import HaskellWorks.Data.Bits.BitPatterns
import HaskellWorks.Data.Bits.BitWise

import qualified Data.ByteString as BS

defaultChunkSize :: Int
defaultChunkSize = 512

newtype BitString = BitString
  { bits :: [BS.ByteString]
  } deriving (Eq)

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
chunkOf0s = BS.replicate defaultChunkSize 0x00
{-# NOINLINE chunkOf0s #-}

chunkOf1s :: BS.ByteString
chunkOf1s = BS.replicate defaultChunkSize 0xff
{-# NOINLINE chunkOf1s #-}
