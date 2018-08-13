{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module HaskellWorks.Data.Bits.BitStrings where

import Control.Monad.Primitive          (PrimMonad)
import Data.Bool
import Data.Semigroup                   (Semigroup (..))
import Data.Word
import GHC.Generics
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitString (BitString (BitString))
import HaskellWorks.Data.Container
import HaskellWorks.Data.Length
import Prelude                          hiding (length)

import qualified Data.ByteString                  as BS
import qualified Data.Vector.Storable             as DVS
import qualified Data.Vector.Storable.Mutable     as DVSM
import qualified HaskellWorks.Data.Bits.BitString as BIS

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
  toBitStrings v = BitStrings [BIS.vectorWord8 v]
  {-# INLINE toBitStrings #-}

instance ToBitStrings (DVS.Vector Word16) where
  toBitStrings v = BitStrings [BIS.vectorWord16 v]
  {-# INLINE toBitStrings #-}

instance ToBitStrings (DVS.Vector Word32) where
  toBitStrings v = BitStrings [BIS.vectorWord32 v]
  {-# INLINE toBitStrings #-}

instance ToBitStrings (DVS.Vector Word64) where
  toBitStrings v = BitStrings [BIS.vectorWord64 v]
  {-# INLINE toBitStrings #-}

instance ToBitStrings BS.ByteString where
  toBitStrings v = BitStrings [BIS.byteString v]
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
zero = BitStrings [BIS.zero]
{-# INLINE zero #-}

one :: BitStrings
one = BitStrings [BIS.one]
{-# INLINE one #-}

bit :: Bool -> BitStrings
bit b = BitStrings [BIS.bit b]
{-# INLINE bit #-}

word8 :: Word8 -> BitStrings
word8 w = BitStrings [BIS.word8 w]
{-# INLINE word8 #-}

word16 :: Word16 -> BitStrings
word16 w = BitStrings [BIS.word16 w]
{-# INLINE word16 #-}

word32 :: Word32 -> BitStrings
word32 w = BitStrings [BIS.word32 w]
{-# INLINE word32 #-}

word64 :: Word64 -> BitStrings
word64 w = BitStrings [BIS.word64 w]
{-# INLINE word64 #-}

vectorWord8 :: DVS.Vector Word8 -> BitStrings
vectorWord8 v = BitStrings [BIS.vectorWord8 v]
{-# INLINE vectorWord8 #-}

vectorWord16 :: DVS.Vector Word16 -> BitStrings
vectorWord16 v = BitStrings [BIS.vectorWord16 v]
{-# INLINE vectorWord16 #-}

vectorWord32 :: DVS.Vector Word32 -> BitStrings
vectorWord32 v = BitStrings [BIS.vectorWord32 v]
{-# INLINE vectorWord32 #-}

vectorWord64 :: DVS.Vector Word64 -> BitStrings
vectorWord64 v = BitStrings [BIS.vectorWord64 v]
{-# INLINE vectorWord64 #-}

byteString :: BS.ByteString -> BitStrings
byteString bs = BitStrings [BIS.byteString bs]
{-# INLINE byteString #-}

foo :: PrimMonad m => Int -> Int -> BitStrings -> DVSM.MVector s Word8 -> m BitStrings
foo i z biss v = undefined -- moo i z 0 biss v

-- moo :: PrimMonad m => Int -> Int -> Word8 -> BitStrings -> DVSM.MVector s Word8 -> m BitStrings
-- moo i z w biss v = if i < z
--   then case biss of
--     BitStrings (cis:ciss) -> do
--       moo i z (BitStrings ciss) v
--     BitStrings [] -> do
--       let vi = i `div` 8
--       let vj = i + 7 `div` 8
--       when (vi /= vj) $ do
--         return ()
--       unsafeMemSet v vi vj 0
--       return mempty
--   else return biss

unsafeMemSet :: PrimMonad m => DVSM.MVector s Word8 -> Int -> Word8 -> m ()
unsafeMemSet = undefined

rechunkBytes :: Int -> BitStrings -> BitStrings
rechunkBytes n bis = undefined
  where go :: Int -> BitStrings -> BitStrings -> BitStrings
        go i ts us = undefined

        writeBitString :: PrimMonad m => Int -> Int -> Word8 -> BitString -> BitStrings -> DVSM.MVector s Word8 -> m BitStrings
        writeBitString i z w bis biss v = if i < z
          then do
            let BitString abs obs lbs = bis
            undefined
          else undefined


