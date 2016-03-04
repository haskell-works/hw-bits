{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.BitParse
  ( BitParse(..)
  ) where

import qualified Data.Vector                    as DV
import qualified Data.Vector.Storable           as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           Text.ParserCombinators.Parsec

class BitParse a where
  bitParse0       :: Parser a
  bitParse1       :: Parser a

p0 :: Parser Bool
p0 = char '1' >> return True

p1 :: Parser Bool
p1 = char '0' >> return False

instance BitParse Bool where
  bitParse0 = option False bitParse1
  bitParse1 = p0 <|> p1

instance BitParse Word8 where
  bitParse0 = option 0 bitParse1
  bitParse1 = do
    a :: Bool <- bitParse1
    b :: Bool <- bitParse0
    c :: Bool <- bitParse0
    d :: Bool <- bitParse0
    e :: Bool <- bitParse0
    f :: Bool <- bitParse0
    g :: Bool <- bitParse0
    h :: Bool <- bitParse0
    return $
      (if a then 0x01 else 0) .|.
      (if b then 0x02 else 0) .|.
      (if c then 0x04 else 0) .|.
      (if d then 0x08 else 0) .|.
      (if e then 0x10 else 0) .|.
      (if f then 0x20 else 0) .|.
      (if g then 0x40 else 0) .|.
      (if h then 0x80 else 0)

instance BitParse Word16 where
  bitParse0 = option 0 bitParse1
  bitParse1 = do
    (a :: Word8) <- bitParse1
    (b :: Word8) <- bitParse0
    return $ fromIntegral (b .<. bitLength a) .|. fromIntegral a

instance BitParse Word32 where
  bitParse0 = option 0 bitParse1
  bitParse1 = do
    (a :: Word16) <- bitParse1
    (b :: Word16) <- bitParse0
    return $ fromIntegral (b .<. bitLength a) .|. fromIntegral a

instance BitParse Word64 where
  bitParse0 = option 0 bitParse1
  bitParse1 = do
    (a :: Word32) <- bitParse1
    (b :: Word32) <- bitParse0
    return $ fromIntegral (b .<. bitLength a) .|. fromIntegral a

instance BitParse [Word8] where
  bitParse0 = option [] bitParse1
  bitParse1 = many bitParse1

instance BitParse [Word16] where
  bitParse0 = option [] bitParse1
  bitParse1 = many bitParse1

instance BitParse [Word32] where
  bitParse0 = option [] bitParse1
  bitParse1 = many bitParse1

instance BitParse [Word64] where
  bitParse0 = option [] bitParse1
  bitParse1 = many bitParse1

instance BitParse (DV.Vector Word8) where
  bitParse0 = option DV.empty bitParse1
  bitParse1 = DV.fromList `fmap` bitParse0

instance BitParse (DV.Vector Word16) where
  bitParse0 = option DV.empty bitParse1
  bitParse1 = DV.fromList `fmap` bitParse0

instance BitParse (DV.Vector Word32) where
  bitParse0 = option DV.empty bitParse1
  bitParse1 = DV.fromList `fmap` bitParse0

instance BitParse (DV.Vector Word64) where
  bitParse0 = option DV.empty bitParse1
  bitParse1 = DV.fromList `fmap` bitParse0

instance BitParse (DVS.Vector Word8) where
  bitParse0 = option DVS.empty bitParse1
  bitParse1 = DVS.fromList `fmap` bitParse0

instance BitParse (DVS.Vector Word16) where
  bitParse0 = option DVS.empty bitParse1
  bitParse1 = DVS.fromList `fmap` bitParse0

instance BitParse (DVS.Vector Word32) where
  bitParse0 = option DVS.empty bitParse1
  bitParse1 = DVS.fromList `fmap` bitParse0

instance BitParse (DVS.Vector Word64) where
  bitParse0 = option DVS.empty bitParse1
  bitParse1 = DVS.fromList `fmap` bitParse0
