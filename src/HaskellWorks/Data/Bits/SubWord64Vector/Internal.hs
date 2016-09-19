module HaskellWorks.Data.Bits.SubWord64Vector.Internal
  ( loBitsSized
  , packBits
  , packBits'
  , unpackBits
  , unpackBits'
  ) where

import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.FixedBitSize

{-# ANN module ("HLint: Reduce duplication" :: String) #-}

class LoBitsSized a where
  loBitsSized :: Int -> a

class Integral a => PackBits a where
  packBits :: Int -> [a] -> [a]
  packBits = packBits' 0 0

  packBits' :: Int -> a -> Int -> [a] -> [a]

class Integral a => UnpackBits a where
  unpackBits :: Int -> Int -> [a] -> [a]
  unpackBits = unpackBits' 0 0

  unpackBits' :: Int -> a -> Int -> Int -> [a] -> [a]

instance LoBitsSized Word64 where
  loBitsSized n = let o = fromIntegral (64 - n) in 0xFFFFFFFFFFFFFFFF .<. o .>. o

instance LoBitsSized Word32 where
  loBitsSized n = let o = fromIntegral (32 - n) in 0xFFFFFFFF .<. o .>. o

instance LoBitsSized Word16 where
  loBitsSized n = let o = fromIntegral (16 - n) in 0xFFFF .<. o .>. o

instance LoBitsSized Word8 where
  loBitsSized n = let o = fromIntegral (8 - n) in 0xFF .<. o .>. o

instance PackBits Word64 where
  packBits' filled carry bitLen (w:ws) = if fillNeeded < fromIntegral (fixedBitSize carry)
      then packBits' fillNeeded newV bitLen ws
      else newV : packBits' fillLeft carryV bitLen ws
    where fillNeeded  = filled + bitLen
          fillMet     = fillNeeded `min` fromIntegral (fixedBitSize carry)
          fillLeft    = fillNeeded - fillMet
          bitMet      = fillMet - filled
          newV        = carry .|. ((w .&. loBitsSized bitMet) .<. fromIntegral filled)
          carryV      = w .>. fromIntegral bitMet
  packBits' _ carry _ _ = [carry]

instance UnpackBits Word64 where
  unpackBits' _ _ 0 _ _ = []
  unpackBits' filled carry dataLen bitLen ws | filled >= bitLen =
    let result = (carry .&. loBitsSized bitLen) : unpackBits' (filled - bitLen) (carry .>. fromIntegral bitLen) (dataLen - 1) bitLen ws in
    result
  unpackBits' filled carry dataLen bitLen (w:ws) =
    let bitsNeeded = bitLen - filled                    in
    let newValue = carry .|. ((w .&. loBitsSized bitsNeeded) .<. fromIntegral filled) in
    newValue : unpackBits' (fromIntegral (fixedBitSize carry) - bitsNeeded) (w .>. fromIntegral bitsNeeded) (dataLen - 1) bitLen ws
  unpackBits' _ _ _ _ _ = []

instance PackBits Word8 where
  packBits' filled carry bitLen (w:ws) = if fillNeeded < fromIntegral (fixedBitSize carry)
      then packBits' fillNeeded newV bitLen ws
      else newV : packBits' fillLeft carryV bitLen ws
    where fillNeeded  = filled + bitLen
          fillMet     = fillNeeded `min` fromIntegral (fixedBitSize carry)
          fillLeft    = fillNeeded - fillMet
          bitMet      = fillMet - filled
          newV        = carry .|. ((w .&. loBitsSized bitMet) .<. fromIntegral filled)
          carryV      = w .>. fromIntegral bitMet
  packBits' _ carry _ _ = [carry]

instance UnpackBits Word8 where
  unpackBits' _ _ 0 _ _ = []
  unpackBits' filled carry dataLen bitLen ws | filled >= bitLen =
    (carry .&. loBitsSized bitLen) : unpackBits' (filled - bitLen) (carry .>. fromIntegral bitLen) (dataLen - 1) bitLen ws
  unpackBits' filled carry dataLen bitLen (w:ws) =
    let bitsNeeded = bitLen - filled                    in
    let newValue = carry .|. ((w .&. loBitsSized bitsNeeded) .<. fromIntegral filled) in
    let result = newValue : unpackBits' (8 - bitsNeeded) (w .>. fromIntegral bitsNeeded) (dataLen - 1) bitLen ws in
    result
  unpackBits' _ _ _ _ _ = []
