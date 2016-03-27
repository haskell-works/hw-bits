{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving         #-}

module HaskellWorks.Data.Bits.BitShown
  ( BitShown(..)
  , bitShown
  ) where

import           HaskellWorks.Data.Bits.BitRead
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitWise

newtype BitShown a = BitShown a deriving (Eq, BitRead, BitShow)

deriving instance Functor BitShown

instance BitShow a => Show (BitShown a) where
  show a = bitShows a ""

bitShown :: BitShown a -> a
bitShown (BitShown a) = a

deriving instance TestBit a => TestBit (BitShown a)
