{-# LANGUAGE DeriveGeneric #-}

module HaskellWorks.Data.Bits.BitStrings.Builder where

import Data.Monoid                       (Monoid (..))
import Data.Semigroup                    (Semigroup (..))
import GHC.Generics
import HaskellWorks.Data.Bits.BitString
import HaskellWorks.Data.Bits.BitStrings

newtype Builder = Builder
  { builder :: BitStrings -> BitStrings
  } deriving Generic

instance Semigroup Builder where
  Builder a <> Builder b = Builder (a <> b)
  {-# INLINE (<>) #-}

instance Monoid Builder where
  mempty = empty
  Builder a `mappend` Builder b = Builder (a <> b)
  {-# INLINE mempty #-}

instance ToBitStrings Builder where
  toBitStrings (Builder b) = b mempty
  {-# INLINE toBitStrings #-}

empty :: Builder
empty = Builder mempty
{-# INLINE empty #-}

bitString :: BitString -> Builder
bitString bs = Builder (toBitStrings bs <>)
{-# INLINE bitString #-}

bitStrings :: BitStrings -> Builder
bitStrings bss = Builder (bss <>)
{-# INLINE bitStrings #-}
