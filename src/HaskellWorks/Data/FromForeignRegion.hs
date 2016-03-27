module HaskellWorks.Data.FromForeignRegion where

import           Data.Word
import           Foreign.ForeignPtr

class FromForeignRegion a where
  fromForeignRegion :: (ForeignPtr Word8, Int, Int) -> a
