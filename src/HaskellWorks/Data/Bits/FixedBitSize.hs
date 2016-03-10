-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.

module HaskellWorks.Data.Bits.FixedBitSize
    ( FixedBitSize(..)
    ) where

import           Data.Word
import           HaskellWorks.Data.Positioning

class FixedBitSize a where
  fixxedBitSize :: a -> Count

instance FixedBitSize Bool where
  fixxedBitSize _ = 1

instance FixedBitSize Word8 where
  fixxedBitSize _ = 8

instance FixedBitSize Word16 where
  fixxedBitSize _ = 16

instance FixedBitSize Word32 where
  fixxedBitSize _ = 32

instance FixedBitSize Word64 where
  fixxedBitSize _ = 64
