{-# LANGUAGE TypeFamilies #-}

module HaskellWorks.Data.Bits.PackedVector.PackedVector64
  ( PackedVector64(..)
  , fromList
  , toList
  ) where

import qualified Data.Vector.Storable as DVS
import           Data.Int
import           Data.Word
import           HaskellWorks.Data.AtIndex
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.LoBitsSized
import           HaskellWorks.Data.Bits.PackedVector.Internal
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Unsign
import           Prelude hiding (length)

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

instance Container PackedVector64 where
  type Elem PackedVector64 = Word64

instance Length PackedVector64 where
  length = fromIntegral . swBufferLen
  {-# INLINE length #-}

instance AtIndex PackedVector64 where
  atIndex v i =
    let bitSize     = fromIntegral (swBitSize v) :: Count
        bitIndex    = fromIntegral (swBitSize v) * i
        (q, r)      = bitIndex `quotRem` 64
        vv          = swBuffer v
    in if r <= 64 - fromIntegral bitSize
      then -- Not crossing boundary
        ((vv !!! q) .>. unsign r) .&. loBitsSized bitSize
      else -- Crossing boundary
        let loBitsSize  = 64 - toCount r
            hiBitsSize  = bitSize - loBitsSize
            loBits      = ((vv !!! q) .>. unsign r) .&. loBitsSized loBitsSize
            hiBits      = if r > 64 - fromIntegral bitSize then (vv !!! (q + 1)) .&. loBitsSized hiBitsSize else 0
        in  loBits .|. (hiBits .<. loBitsSize)
  (!!!)       = atIndex
  {-# INLINE (!!!)   #-}
  {-# INLINE atIndex #-}

fromList :: Count -> [Word64] -> PackedVector64
fromList wl ws =
  PackedVector64
  { swBuffer    = DVS.fromList (packBits wl ws)
  , swBufferLen = fromIntegral (length ws)
  , swBitSize   = fromIntegral wl
  }

toList :: PackedVector64 -> [Word64]
toList v = unpackBits (swBufferLen v) (fromIntegral (swBitSize v)) (DVS.toList (swBuffer v))
