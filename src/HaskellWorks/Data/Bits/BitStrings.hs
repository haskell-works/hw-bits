{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies  #-}

module HaskellWorks.Data.Bits.BitStrings where

import Data.Semigroup                   (Semigroup (..))
import GHC.Generics
import HaskellWorks.Data.Bits.BitShow
import HaskellWorks.Data.Bits.BitString
import HaskellWorks.Data.Container
import HaskellWorks.Data.Length
import Prelude                          hiding (length)

newtype BitStrings = BitStrings
  { chunks :: [BitString]
  } deriving (Generic)

instance Semigroup BitStrings where
  BitStrings as <> BitStrings bs = BitStrings (as <> bs)

instance Monoid BitStrings where
  mempty = BitStrings mempty
  BitStrings as `mappend` BitStrings bs = BitStrings (as <> bs)
  {-# INLINE mempty #-}

class ToBitStrings a where
  toBitStrings :: a -> BitStrings

instance ToBitStrings BitStrings where
  toBitStrings = id
  {-# INLINE toBitStrings #-}

instance ToBitStrings BitString where
  toBitStrings bs = BitStrings [bs]
  {-# INLINE toBitStrings #-}

instance Container BitStrings where
  type Elem BitStrings = Bool

instance Length BitStrings where
  length (BitStrings bss) = sum (length <$> bss)
  end (BitStrings bss) = sum (end <$> bss)
  {-# INLINE end #-}

instance BitShow BitStrings where
  bitShows (BitStrings bss) = mconcat $ bitShows <$> bss
  {-# INLINE bitShows #-}
