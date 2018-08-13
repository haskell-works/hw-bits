{-# LANGUAGE FlexibleInstances #-}

module HaskellWorks.Data.Bits.BitString
  ( ToBitString(..)
  , BitString(BitString)
  , empty
  , zero
  , one
  , bit
  , word8
  , word16
  , word32
  , word64
  , vectorWord8
  , vectorWord16
  , vectorWord32
  , vectorWord64
  , byteString
  ) where

import Data.Bool
import Data.Word
import HaskellWorks.Data.Bits.BitString.Type
import HaskellWorks.Data.Vector.AsVector8
import Prelude                               hiding (length)

import qualified Data.ByteString          as BS
import qualified Data.Vector.Storable     as DVS
import qualified HaskellWorks.Data.Length as HW

empty :: BitString
empty = BitString
  { vector = DVS.empty
  , offset = 0
  , length = 0
  }
{-# INLINE empty #-}

class ToBitString a where
  toBitString :: a -> BitString

instance ToBitString BitString where
  toBitString = id
  {-# INLINE toBitString #-}

instance ToBitString (DVS.Vector Word8) where
  toBitString = vectorWord8
  {-# INLINE toBitString #-}

instance ToBitString (DVS.Vector Word16) where
  toBitString = vectorWord16
  {-# INLINE toBitString #-}

instance ToBitString (DVS.Vector Word32) where
  toBitString = vectorWord32
  {-# INLINE toBitString #-}

instance ToBitString (DVS.Vector Word64) where
  toBitString = vectorWord64
  {-# INLINE toBitString #-}

instance ToBitString BS.ByteString where
  toBitString = byteString
  {-# INLINE toBitString #-}

instance ToBitString Bool where
  toBitString = bit
  {-# INLINE toBitString #-}

instance ToBitString Word8 where
  toBitString = word8
  {-# INLINE toBitString #-}

instance ToBitString Word16 where
  toBitString = word16
  {-# INLINE toBitString #-}

instance ToBitString Word32 where
  toBitString = word32
  {-# INLINE toBitString #-}

instance ToBitString Word64 where
  toBitString = word64
  {-# INLINE toBitString #-}

zero :: BitString
zero = BitString (DVS.singleton 0) 0 1
{-# INLINE zero #-}

one :: BitString
one = BitString (DVS.singleton 1) 0 1
{-# INLINE one #-}

bit :: Bool -> BitString
bit = bool zero one
{-# INLINE bit #-}

word8 :: Word8 -> BitString
word8 w = BitString (DVS.singleton w) 0 8
{-# INLINE word8 #-}

word16 :: Word16 -> BitString
word16 w = BitString (DVS.unsafeCast (DVS.singleton w)) 0 16
{-# INLINE word16 #-}

word32 :: Word32 -> BitString
word32 w = BitString (DVS.unsafeCast (DVS.singleton w)) 0 32
{-# INLINE word32 #-}

word64 :: Word64 -> BitString
word64 w = BitString (DVS.unsafeCast (DVS.singleton w)) 0 64
{-# INLINE word64 #-}

vectorWord8 :: DVS.Vector Word8 -> BitString
vectorWord8 v = BitString
  { vector = v
  , offset = 0
  , length = HW.length v * 8
  }
{-# INLINE vectorWord8 #-}

vectorWord16 :: DVS.Vector Word16 -> BitString
vectorWord16 = vectorWord8 . DVS.unsafeCast
{-# INLINE vectorWord16 #-}

vectorWord32 :: DVS.Vector Word32 -> BitString
vectorWord32 = vectorWord8 . DVS.unsafeCast
{-# INLINE vectorWord32 #-}

vectorWord64 :: DVS.Vector Word64 -> BitString
vectorWord64 = vectorWord8 . DVS.unsafeCast
{-# INLINE vectorWord64 #-}

byteString :: BS.ByteString -> BitString
byteString = vectorWord8 . asVector8
{-# INLINE byteString #-}
