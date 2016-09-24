module HaskellWorks.Data.Bits.SubWord64Vector
  ( SubWord64Vector(..)
  , fromList
  , toList
  ) where

import qualified Data.Vector.Storable as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.SubWord64Vector.Internal

data SubWord64Vector = SubWord64Vector
    { swBuffer      :: !(DVS.Vector Word64)
    , swBitSize     :: !Word
    , swBufferLen   :: !Int
    } deriving (Eq, Show)

-- empty :: SubWord64Vector
-- empty =
--   SubWord64Vector
--   { swBuffer    = DVS.empty
--   , swBufferLen = 0
--   , swBitSize   = 1
--   }
--
fromList :: Int -> [Word64] -> SubWord64Vector
fromList wl ws =
  SubWord64Vector
  { swBuffer    = DVS.fromList (packBits wl ws)
  , swBufferLen = fromIntegral (length ws)
  , swBitSize   = fromIntegral wl
  }

toList :: SubWord64Vector -> [Word64]
toList v = unpackBits (swBufferLen v) (fromIntegral (swBitSize v)) (DVS.toList (swBuffer v))
