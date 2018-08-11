{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies      #-}

module HaskellWorks.Data.Bits.BitString.Type where

import Data.Word
import GHC.Generics
import HaskellWorks.Data.AtIndex      hiding (length)
import HaskellWorks.Data.Bits.BitRead
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Drop
import HaskellWorks.Data.Positioning
import Prelude                        hiding (length)

import qualified Data.Vector.Storable     as DVS
import qualified HaskellWorks.Data.Length as HW

data BitString = BitString
  { vector :: !(DVS.Vector Word64)
  , offset :: !Position
  , length :: !Count
  } deriving (Generic)

instance ToBitString (DVS.Vector Word64) where
  toBitString v = BitString
    { vector = v
    , offset = 0
    , length = HW.length v * 64
    }
  {-# INLINE toBitString #-}

empty :: BitString
empty = BitString
  { vector = DVS.empty
  , offset = 0
  , length = 0
  }
{-# INLINE empty #-}

class ToBitString a where
  toBitString :: a -> BitString

instance ToBitString BitString where
  toBitString = id
  {-# INLINE toBitString #-}

instance Container BitString where
  type Elem BitString = Bool

instance HW.Length BitString where
  length = length
  {-# INLINE length #-}

instance BitShow BitString where
  bitShows (BitString av ao al) = go ao
    where bo = fromIntegral ao :: Position
          be = bo + fromIntegral al :: Position
          go :: Position -> String -> String
          go i = if i < be
            then if (av !!! (i `div` 64)) .?. (i `mod` 64)
              then ('1':) . go (i + 1)
              else ('0':) . go (i + 1)
            else id
  {-# INLINE bitShows #-}

instance BitRead BitString where
  bitRead s = mkBitString <$> bitRead s
    where mkBitString v = BitString
            { vector = v
            , offset = 0
            , length = HW.length $ filter (\c -> c == '1' || c == '0') s
            }
  {-# INLINE bitRead #-}

instance Drop BitString where
  drop n bs = bs
    { offset = offset bs + fromIntegral delta
    , length = length bs - delta
    }
    where delta = n `min` length bs
  {-# INLINE drop #-}
