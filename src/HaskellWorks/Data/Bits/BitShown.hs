{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Bits.BitShown
  ( BitShown(..)
  ) where

import           HaskellWorks.Data.Bits.BitRead
import           HaskellWorks.Data.Bits.BitShow

newtype BitShown a = BitShown a deriving (Eq, BitRead, BitShow)

instance BitShow a => Show (BitShown a) where
  show a = bitShows a ""
