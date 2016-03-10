{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.BitWise
    ( -- * Bit map
      BitWise(..)
    , Shift(..)
    , TestBit(..)
    ) where

import qualified Data.Bits                           as B
import qualified Data.Vector                         as DV
import qualified Data.Vector.Storable                as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Vector.VectorLike as VL
import           Prelude                             as P

-- We pervasively use precedence to avoid excessive parentheses, and we use
-- the same precedence conventions of the C programming language: arithmetic
-- operators come first, ordered in the standard way, followed by shifts,
-- followed by logical operators; ⊕ sits between | and &.
infixl 9 .?.
infixl 8 .<., .>.
infixl 7 .&.        -- Bitwise AND.  eg. ∧
infixl 6 .^.        -- Bitwise XOR.  eg. ⊕
infixl 5 .|.        -- Bitwise OR.   eg. ∨

class Shift a where
  (.<.) :: a -> Count -> a
  (.>.) :: a -> Count -> a

class TestBit a where
  (.?.) :: a -> Position -> Bool

class BitWise a where
  (.&.) :: a -> a -> a
  (.|.) :: a -> a -> a
  (.^.) :: a -> a -> a
  comp  :: a -> a
  all0s :: a
  all1s :: a

--------------------------------------------------------------------------------
-- Instances

instance TestBit Bool where
  (.?.) w 0 = w
  (.?.) _ _ = error "Invalid bit index"
  {-# INLINABLE (.?.) #-}

instance TestBit [Bool] where
  (.?.) v p = v !! fromIntegral p
  {-# INLINABLE (.?.) #-}

instance TestBit Word8 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance TestBit Word16 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance TestBit Word32 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance TestBit Word64 where
  (.?.) w n = B.testBit w (fromIntegral (getPosition n))
  {-# INLINABLE (.?.) #-}

instance TestBit (DV.Vector Word8) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance TestBit (DV.Vector Word16) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance TestBit (DV.Vector Word32) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance TestBit (DV.Vector Word64) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance TestBit (DVS.Vector Word8) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance TestBit (DVS.Vector Word16) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance TestBit (DVS.Vector Word32) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance TestBit (DVS.Vector Word64) where
  (.?.) v n = let (q, r) = n `quotRem` elemBitEnd v in (v !!! q) .?. r
  {-# INLINABLE (.?.) #-}

instance BitWise Word8 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

  all0s = 0
  {-# INLINABLE all0s #-}

  all1s = 0
  {-# INLINABLE all1s #-}

instance BitWise Word16 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

  all0s = 0
  {-# INLINABLE all0s #-}

  all1s = 0
  {-# INLINABLE all1s #-}

instance BitWise Word32 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

  all0s = 0
  {-# INLINABLE all0s #-}

  all1s = 0
  {-# INLINABLE all1s #-}

instance BitWise Word64 where
  (.&.) = (B..&.)
  {-# INLINABLE (.&.) #-}

  (.|.) = (B..|.)
  {-# INLINABLE (.|.) #-}

  (.^.) = B.xor
  {-# INLINABLE (.^.) #-}

  comp  = B.complement
  {-# INLINABLE comp #-}

  all0s = 0
  {-# INLINABLE all0s #-}

  all1s = 0
  {-# INLINABLE all1s #-}

instance Shift Word8  where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}

instance Shift Word16 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}

instance Shift Word32 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}

instance Shift Word64 where
  (.<.) w n = B.shiftL w (fromIntegral n)
  {-# INLINABLE (.<.) #-}

  (.>.) w n = B.shiftR w (fromIntegral n)
  {-# INLINABLE (.>.) #-}
