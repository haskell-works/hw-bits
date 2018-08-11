{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module HaskellWorks.Data.Bits.BitStrings where

import Data.Bool
import Data.Semigroup                   (Semigroup (..))
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitString (BitString)
import HaskellWorks.Data.Container
import HaskellWorks.Data.Length
import Prelude                          hiding (length)

import qualified Data.ByteString                  as BS
import qualified Data.Vector.Storable             as DVS
import qualified HaskellWorks.Data.Bits.BitString as BiS

newtype BitStrings = BitStrings
  { chunks :: [BitString]
  } deriving (Generic)

instance Semigroup BitStrings where
  BitStrings as <> BitStrings bs = BitStrings (as <> bs)
  {-# INLINE (<>) #-}

instance Monoid BitStrings where
  mempty = BitStrings mempty
  BitStrings as `mappend` BitStrings bs = BitStrings (as <> bs)
  {-# INLINE mempty #-}
  {-# INLINE mappend #-}

class ToBitStrings a where
  toBitStrings :: a -> BitStrings

instance ToBitStrings BitStrings where
  toBitStrings = id
  {-# INLINE toBitStrings #-}

instance ToBitStrings BitString where
  toBitStrings bs = BitStrings [bs]
  {-# INLINE toBitStrings #-}

instance Container BitStrings where
  type Elem BitStrings = Bool

instance Length BitStrings where
  length (BitStrings bss) = sum (length <$> bss)
  end (BitStrings bss) = sum (end <$> bss)
  {-# INLINE end #-}

instance BitShow BitStrings where
  bitShows (BitStrings bss) = mconcat $ bitShows <$> bss
  {-# INLINE bitShows #-}

instance ToBitStrings (DVS.Vector Word8) where
  toBitStrings v = BitStrings [BiS.vectorWord8 v]
  {-# INLINE toBitStrings #-}

instance ToBitStrings (DVS.Vector Word16) where
  toBitStrings v = BitStrings [BiS.vectorWord16 v]
  {-# INLINE toBitStrings #-}

instance ToBitStrings (DVS.Vector Word32) where
  toBitStrings v = BitStrings [BiS.vectorWord32 v]
  {-# INLINE toBitStrings #-}

instance ToBitStrings (DVS.Vector Word64) where
  toBitStrings v = BitStrings [BiS.vectorWord64 v]
  {-# INLINE toBitStrings #-}

instance ToBitStrings BS.ByteString where
  toBitStrings v = BitStrings [BiS.byteString v]
  {-# INLINE toBitStrings #-}

instance ToBitStrings Bool where
  toBitStrings = bit
  {-# INLINE toBitStrings #-}

instance ToBitStrings Word8 where
  toBitStrings = word8
  {-# INLINE toBitStrings #-}

instance ToBitStrings Word16 where
  toBitStrings = word16
  {-# INLINE toBitStrings #-}

instance ToBitStrings Word32 where
  toBitStrings = word32
  {-# INLINE toBitStrings #-}

instance ToBitStrings Word64 where
  toBitStrings = word64
  {-# INLINE toBitStrings #-}

zero :: BitStrings
zero = BitStrings [BiS.zero]
{-# INLINE zero #-}

one :: BitStrings
one = BitStrings [BiS.one]
{-# INLINE one #-}

bit :: Bool -> BitStrings
bit b = BitStrings [BiS.bit b]
{-# INLINE bit #-}

word8 :: Word8 -> BitStrings
word8 w = BitStrings [BiS.word8 w]
{-# INLINE word8 #-}

word16 :: Word16 -> BitStrings
word16 w = BitStrings [BiS.word16 w]
{-# INLINE word16 #-}

word32 :: Word32 -> BitStrings
word32 w = BitStrings [BiS.word32 w]
{-# INLINE word32 #-}

word64 :: Word64 -> BitStrings
word64 w = BitStrings [BiS.word64 w]
{-# INLINE word64 #-}

vectorWord8 :: DVS.Vector Word8 -> BitStrings
vectorWord8 v = BitStrings [BiS.vectorWord8 v]
{-# INLINE vectorWord8 #-}

vectorWord16 :: DVS.Vector Word16 -> BitStrings
vectorWord16 v = BitStrings [BiS.vectorWord16 v]
{-# INLINE vectorWord16 #-}

vectorWord32 :: DVS.Vector Word32 -> BitStrings
vectorWord32 v = BitStrings [BiS.vectorWord32 v]
{-# INLINE vectorWord32 #-}

vectorWord64 :: DVS.Vector Word64 -> BitStrings
vectorWord64 v = BitStrings [BiS.vectorWord64 v]
{-# INLINE vectorWord64 #-}

byteString :: BS.ByteString -> BitStrings
byteString bs = BitStrings [BiS.byteString bs]
{-# INLINE byteString #-}
