module HaskellWorks.Data.FromForeignRegion where

import           Data.Word
import           Foreign.ForeignPtr

-- | Class for datastructures that can be created from a foreign region
class FromForeignRegion a where
  -- | Create a value of type @a from a foreign region.
  fromForeignRegion :: (ForeignPtr Word8, Int, Int) -> a
