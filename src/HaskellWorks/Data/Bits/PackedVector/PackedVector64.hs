module HaskellWorks.Data.Bits.PackedVector.PackedVector64
  ( PackedVector64(..)
  , fromList
  , toList
  ) where

import qualified Data.Vector.Storable as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.PackedVector.Internal

data PackedVector64 = PackedVector64
    { swBuffer      :: !(DVS.Vector Word64)
    , swBitSize     :: !Word
    , swBufferLen   :: !Int
    } deriving (Eq, Show)

-- empty :: PackedVector64
-- empty =
--   PackedVector64
--   { swBuffer    = DVS.empty
--   , swBufferLen = 0
--   , swBitSize   = 1
--   }
--
fromList :: Int -> [Word64] -> PackedVector64
fromList wl ws =
  PackedVector64
  { swBuffer    = DVS.fromList (packBits wl ws)
  , swBufferLen = fromIntegral (length ws)
  , swBitSize   = fromIntegral wl
  }

toList :: PackedVector64 -> [Word64]
toList v = unpackBits (swBufferLen v) (fromIntegral (swBitSize v)) (DVS.toList (swBuffer v))
