{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Bits.BitStrings.Builder where

import Data.Monoid                       (Monoid (..))
import Data.Semigroup                    (Semigroup (..))
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitString  (BitString)
import HaskellWorks.Data.Bits.BitStrings (BitStrings (BitStrings), ToBitStrings (..))

import qualified Data.ByteString                  as BS
import qualified Data.Vector.Storable             as DVS
import qualified HaskellWorks.Data.Bits.BitString as BiS

newtype Builder = Builder
  { builder :: BitStrings -> BitStrings
  } deriving Generic

instance Semigroup Builder where
  Builder a <> Builder b = Builder (a <> b)
  {-# INLINE (<>) #-}

instance Monoid Builder where
  mempty = empty
  Builder a `mappend` Builder b = Builder (a <> b)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

instance ToBitStrings Builder where
  toBitStrings (Builder b) = b mempty
  {-# INLINE toBitStrings #-}

class ToBuilder a where
  toBuilder :: a -> Builder

instance ToBuilder ([BitString] -> [BitString]) where
  toBuilder f = Builder (\(BitStrings bss) -> BitStrings (f bss))
  {-# INLINE toBuilder #-}

instance ToBuilder (BitStrings -> BitStrings) where
  toBuilder = Builder
  {-# INLINE toBuilder #-}

empty :: Builder
empty = Builder mempty
{-# INLINE empty #-}

zero :: Builder
zero = toBuilder (BiS.zero:)
{-# INLINE zero #-}

one :: Builder
one = toBuilder (BiS.one:)
{-# INLINE one #-}

bitString :: BitString -> Builder
bitString bs = toBuilder (toBitStrings bs <>)
{-# INLINE bitString #-}

bitStrings :: BitStrings -> Builder
bitStrings bss = toBuilder (bss <>)
{-# INLINE bitStrings #-}

word8 :: Word8 -> Builder
word8 w = toBuilder (BiS.word8 w:)
{-# INLINE word8 #-}

word16 :: Word16 -> Builder
word16 w = toBuilder (BiS.word16 w:)
{-# INLINE word16 #-}

word32 :: Word32 -> Builder
word32 w = toBuilder (BiS.word32 w:)
{-# INLINE word32 #-}

word64 :: Word64 -> Builder
word64 w = toBuilder (BiS.word64 w:)
{-# INLINE word64 #-}

vectorWord8 :: DVS.Vector Word8 -> Builder
vectorWord8 v = toBuilder (BiS.vectorWord8 v:)
{-# INLINE vectorWord8 #-}

vectorWord16 :: DVS.Vector Word16 -> Builder
vectorWord16 v = toBuilder (BiS.vectorWord16 v:)
{-# INLINE vectorWord16 #-}

vectorWord32 :: DVS.Vector Word32 -> Builder
vectorWord32 v = toBuilder (BiS.vectorWord32 v:)
{-# INLINE vectorWord32 #-}

vectorWord64 :: DVS.Vector Word64 -> Builder
vectorWord64 v = toBuilder (BiS.vectorWord64 v:)
{-# INLINE vectorWord64 #-}

byteString :: BS.ByteString -> Builder
byteString v = toBuilder (BiS.byteString v:)
{-# INLINE byteString #-}
