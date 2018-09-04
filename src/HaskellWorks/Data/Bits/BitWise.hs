{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module HaskellWorks.Data.Bits.BitWise
    ( -- * Bit map
      BitWise(..)
    , Shift(..)
    , TestBit(..)
    ) where

import Data.Word
import HaskellWorks.Data.AtIndex
import HaskellWorks.Data.Bits.BitLength
import HaskellWorks.Data.Naive
import HaskellWorks.Data.Positioning
import Prelude                          as P

import qualified Data.Bits                           as B
import qualified Data.ByteString                     as BS
import qualified Data.Vector                         as DV
import qualified Data.Vector.Storable                as DVS
import qualified HaskellWorks.Data.AtIndex           as HW
import qualified HaskellWorks.Data.ByteString        as BS
import qualified HaskellWorks.Data.Vector.AsVector64 as DVS
import qualified HaskellWorks.Data.Vector.AsVector8  as DVS

-- We pervasively use precedence to avoid excessive parentheses, and we use
-- the same precedence conventions of the C programming language: arithmetic
-- operators come first, ordered in the standard way, followed by shifts,
-- followed by logical operators; ⊕ sits between | and &.
infixl 9 .?.
infixl 8 .<., .>.
infixl 7 .&.        -- Bitwise AND.  eg. ∧
infixl 6 .^.        -- Bitwise XOR.  eg. ⊕
infixl 5 .|.        -- Bitwise OR.   eg. ∨

-- | Class of values that have shift operations
class Shift a where
  -- | Shift left by the specified count
  (.<.) :: a -> Count -> a
  -- | Shift right by the specified count
  (.>.) :: a -> Count -> a

  -- | Class of values that have bit test operations
class TestBit a where
  -- | Test whether the bit ad the given offset is set
  (.?.) :: a -> Position -> Bool

-- | Class of values that have bit wise logical operations
class BitWise a where
  -- | Bit-wise AND
  (.&.) :: a -> a -> a
  -- | Bit-wise OR
  (.|.) :: a -> a -> a
  -- | Bit-wise XOR
  (.^.) :: a -> a -> a
  -- | Bit-wise complement
  comp  :: a -> a
  -- | Bit-wise value of the given type with all bits set to zero
  all0s :: a
  -- | Bit-wise value of the given type with all bits set to one
  all1s :: a

--------------------------------------------------------------------------------
-- Instances

instance TestBit Bool where
  (.?.) w 0 = w
  (.?.) _ _ = error "Invalid bit index"
  {-# INLINE (.?.) #-}

instance TestBit [Bool] where
  (.?.) v p = v !! fromIntegral p
  {-# INLINE (.?.) #-}

instance TestBit Int where
  (.?.) w n = B.testBit w (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit Word8 where
  (.?.) w n = B.testBit w (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit Word16 where
  (.?.) w n = B.testBit w (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit Word32 where
  (.?.) w n = B.testBit w (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit Word64 where
  (.?.) w n = B.testBit w (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit (Naive Word8) where
  (.?.) w n = B.testBit (naive w) (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit (Naive Word16) where
  (.?.) w n = B.testBit (naive w) (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit (Naive Word32) where
  (.?.) w n = B.testBit (naive w) (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit (Naive Word64) where
  (.?.) w n = B.testBit (naive w) (fromIntegral n)
  {-# INLINE (.?.) #-}

instance TestBit (DV.Vector Word8) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance TestBit (DV.Vector Word16) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance TestBit (DV.Vector Word32) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance TestBit (DV.Vector Word64) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance TestBit (DVS.Vector Word8) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance TestBit (DVS.Vector Word16) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance TestBit (DVS.Vector Word32) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance TestBit (DVS.Vector Word64) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINE (.?.) #-}

instance BitWise Int where
  (.&.) = (B..&.)
  {-# INLINE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINE (.|.) #-}

  (.^.) = B.xor
  {-# INLINE (.^.) #-}

  comp  = B.complement
  {-# INLINE comp #-}

  all0s = 0
  {-# INLINE all0s #-}

  all1s = -1
  {-# INLINE all1s #-}

instance BitWise Word8 where
  (.&.) = (B..&.)
  {-# INLINE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINE (.|.) #-}

  (.^.) = B.xor
  {-# INLINE (.^.) #-}

  comp  = B.complement
  {-# INLINE comp #-}

  all0s = 0
  {-# INLINE all0s #-}

  all1s = 0xff
  {-# INLINE all1s #-}

instance BitWise Word16 where
  (.&.) = (B..&.)
  {-# INLINE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINE (.|.) #-}

  (.^.) = B.xor
  {-# INLINE (.^.) #-}

  comp  = B.complement
  {-# INLINE comp #-}

  all0s = 0
  {-# INLINE all0s #-}

  all1s = 0xffff
  {-# INLINE all1s #-}

instance BitWise Word32 where
  (.&.) = (B..&.)
  {-# INLINE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINE (.|.) #-}

  (.^.) = B.xor
  {-# INLINE (.^.) #-}

  comp  = B.complement
  {-# INLINE comp #-}

  all0s = 0
  {-# INLINE all0s #-}

  all1s = 0xffffffff
  {-# INLINE all1s #-}

instance BitWise Word64 where
  (.&.) = (B..&.)
  {-# INLINE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINE (.|.) #-}

  (.^.) = B.xor
  {-# INLINE (.^.) #-}

  comp  = B.complement
  {-# INLINE comp #-}

  all0s = 0
  {-# INLINE all0s #-}

  all1s = 0xffffffffffffffff
  {-# INLINE all1s #-}

instance BitWise (DVS.Vector Word8) where
  as .&. bs = DVS.constructN (fromIntegral len) go
    where len = DVS.length as `min` DVS.length bs
          go v = (vas !!! vi) .&. (vbs !!! vi)
            where vi = HW.end v
                  vas = DVS.asVector8 as
                  vbs = DVS.asVector8 bs
  {-# INLINE (.&.) #-}

  as .|. bs = DVS.constructN (fromIntegral len) go
    where len = DVS.length as `min` DVS.length bs
          go v = (vas !!! vi) .|. (vbs !!! vi)
            where vi = HW.end v
                  vas = DVS.asVector8 as
                  vbs = DVS.asVector8 bs
  {-# INLINE (.|.) #-}

  as .^. bs = DVS.constructN (fromIntegral len) go
    where len = DVS.length as `min` DVS.length bs
          go v = (vas !!! vi) .^. (vbs !!! vi)
            where vi = HW.end v
                  vas = DVS.asVector8 as
                  vbs = DVS.asVector8 bs
  {-# INLINE (.^.) #-}

  comp = DVS.map comp
  {-# INLINE comp #-}

  all0s = DVS.empty
  {-# INLINE all0s #-}

  all1s = DVS.empty
  {-# INLINE all1s #-}

instance BitWise (DVS.Vector Word64) where
  as .&. bs = DVS.constructN (fromIntegral len) go
    where len = DVS.length as `min` DVS.length bs
          go v = (vas !!! vi) .&. (vbs !!! vi)
            where vi = HW.end v
                  vas = DVS.asVector64 as
                  vbs = DVS.asVector64 bs
  {-# INLINE (.&.) #-}

  as .|. bs = DVS.constructN (fromIntegral len) go
    where len = DVS.length as `min` DVS.length bs
          go v = (vas !!! vi) .|. (vbs !!! vi)
            where vi = HW.end v
                  vas = DVS.asVector64 as
                  vbs = DVS.asVector64 bs
  {-# INLINE (.|.) #-}

  as .^. bs = DVS.constructN (fromIntegral len) go
    where len = DVS.length as `min` DVS.length bs
          go v = (vas !!! vi) .^. (vbs !!! vi)
            where vi = HW.end v
                  vas = DVS.asVector64 as
                  vbs = DVS.asVector64 bs
  {-# INLINE (.^.) #-}

  comp = DVS.map comp
  {-# INLINE comp #-}

  all0s = DVS.empty
  {-# INLINE all0s #-}

  all1s = DVS.empty
  {-# INLINE all1s #-}

instance BitWise BS.ByteString where
  as .&. bs = if (len `div` 8) * 8 == len
    then BS.toByteString (DVS.asVector64 as .&. DVS.asVector64 bs)
    else BS.toByteString (DVS.asVector8  as .&. DVS.asVector8  bs)
    where len = BS.length as `min` BS.length bs
  {-# INLINE (.&.) #-}

  as .|. bs = if (len `div` 8) * 8 == len
    then BS.toByteString (DVS.asVector64 as .|. DVS.asVector64 bs)
    else BS.toByteString (DVS.asVector8  as .|. DVS.asVector8  bs)
    where len = BS.length as `min` BS.length bs
  {-# INLINE (.|.) #-}

  as .^. bs = if (len `div` 8) * 8 == len
    then BS.toByteString (DVS.asVector64 as .^. DVS.asVector64 bs)
    else BS.toByteString (DVS.asVector8  as .^. DVS.asVector8  bs)
    where len = BS.length as `min` BS.length bs
  {-# INLINE (.^.) #-}

  comp as = if (len `div` 8) * 8 == len
    then BS.toByteString (comp (DVS.asVector64 as))
    else BS.toByteString (comp (DVS.asVector8  as))
    where len = BS.length as
  {-# INLINE comp #-}

  all0s = BS.empty
  {-# INLINE all0s #-}

  all1s = BS.empty
  {-# INLINE all1s #-}

instance Shift Int  where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINE (.>.) #-}

instance Shift Word8  where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINE (.>.) #-}

instance Shift Word16 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINE (.>.) #-}

instance Shift Word32 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINE (.>.) #-}

instance Shift Word64 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINE (.>.) #-}
