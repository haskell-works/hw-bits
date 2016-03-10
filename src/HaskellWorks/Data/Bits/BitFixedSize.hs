-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.

module HaskellWorks.Data.Bits.BitFixedSize
    ( BitFixedSize(..)
    ) where

import           Data.Word
import           HaskellWorks.Data.Positioning

class BitFixedSize a where
  bitFixedSize :: a -> Count

instance BitFixedSize Bool where
  bitFixedSize _ = 1

instance BitFixedSize Word8 where
  bitFixedSize _ = 8

instance BitFixedSize Word16 where
  bitFixedSize _ = 16

instance BitFixedSize Word32 where
  bitFixedSize _ = 32

instance BitFixedSize Word64 where
  bitFixedSize _ = 64
