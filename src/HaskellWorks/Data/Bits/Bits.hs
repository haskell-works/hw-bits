module HaskellWorks.Data.Bits.Bits where

import qualified Data.ByteString.Lazy.Internal as LBSI

defaultChunkBytes :: Int
defaultChunkBytes = (LBSI.defaultChunkSize `div` 64) * 64
{-# INLINE defaultChunkBytes #-}

defaultChunkWord64s :: Int
defaultChunkWord64s = defaultChunkBytes `div` 8
{-# INLINE defaultChunkWord64s #-}
