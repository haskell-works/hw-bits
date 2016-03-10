{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.

module HaskellWorks.Data.Bits.ElemFixedBitSize
    ( ElemFixedBitSize(..)
    ) where

import qualified Data.Vector                   as DV
import qualified Data.Vector.Storable          as DVS
import           Data.Word
import           HaskellWorks.Data.Positioning

class ElemFixedBitSize v where
  type Elem v
  elemFixedBitSize :: v -> Count

instance ElemFixedBitSize [Bool] where
  type Elem [Bool] = Bool
  elemFixedBitSize _ = 1

instance ElemFixedBitSize [Word8] where
  type Elem [Word8] = Word8
  elemFixedBitSize _ = 8

instance ElemFixedBitSize [Word16] where
  type Elem [Word16] = Word16
  elemFixedBitSize _ = 16

instance ElemFixedBitSize [Word32] where
  type Elem [Word32] = Word32
  elemFixedBitSize _ = 32

instance ElemFixedBitSize [Word64] where
  type Elem [Word64] = Word64
  elemFixedBitSize _ = 64

instance ElemFixedBitSize (DV.Vector Bool) where
  type Elem (DV.Vector Bool) = Bool
  elemFixedBitSize _ = 1

instance ElemFixedBitSize (DV.Vector Word8) where
  type Elem (DV.Vector Word8) = Word8
  elemFixedBitSize _ = 8

instance ElemFixedBitSize (DV.Vector Word16) where
  type Elem (DV.Vector Word16) = Word16
  elemFixedBitSize _ = 16

instance ElemFixedBitSize (DV.Vector Word32) where
  type Elem (DV.Vector Word32) = Word32
  elemFixedBitSize _ = 32

instance ElemFixedBitSize (DV.Vector Word64) where
  type Elem (DV.Vector Word64) = Word64
  elemFixedBitSize _ = 64

instance ElemFixedBitSize (DVS.Vector Bool) where
  type Elem (DVS.Vector Bool) = Bool
  elemFixedBitSize _ = 1

instance ElemFixedBitSize (DVS.Vector Word8) where
  type Elem (DVS.Vector Word8) = Word8
  elemFixedBitSize _ = 8

instance ElemFixedBitSize (DVS.Vector Word16) where
  type Elem (DVS.Vector Word16) = Word16
  elemFixedBitSize _ = 16

instance ElemFixedBitSize (DVS.Vector Word32) where
  type Elem (DVS.Vector Word32) = Word32
  elemFixedBitSize _ = 32

instance ElemFixedBitSize (DVS.Vector Word64) where
  type Elem (DVS.Vector Word64) = Word64
  elemFixedBitSize _ = 64

