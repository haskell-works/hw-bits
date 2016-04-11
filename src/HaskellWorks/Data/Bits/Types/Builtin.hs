{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Bits.Types.Builtin where

import Data.Vector.Storable

newtype Builtin a = Builtin a deriving (Eq, Show, Storable)
