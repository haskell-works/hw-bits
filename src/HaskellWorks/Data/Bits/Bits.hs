{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms   #-}

module HaskellWorks.Data.Bits.Bits
  ( pattern (:~)
  , pattern Nil
  , pattern BS

  , defaultChunkBytes
  , defaultChunkWord64s

  ) where

import Data.Word

import qualified Data.ByteString                   as BS
import qualified Data.ByteString.Internal          as BSI
import qualified Data.ByteString.Lazy              as LBS
import qualified Data.ByteString.Lazy.Internal     as LBSI
import qualified Data.Vector.Storable              as DVS
import qualified HaskellWorks.Data.ByteString      as BS
import qualified HaskellWorks.Data.ByteString.Lazy as LBS

pattern (:~) :: BSI.ByteString -> LBS.ByteString -> LBS.ByteString
pattern (:~) bs bss = LBSI.Chunk bs bss

pattern Nil :: LBS.ByteString
pattern Nil = LBSI.Empty

pattern BS :: LBS.ByteString -> BitString
pattern BS bs = BitString bs

infixr  6 :~

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
