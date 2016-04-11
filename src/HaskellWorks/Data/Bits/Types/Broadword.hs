{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Bits.Types.Broadword where

import Data.Vector.Storable

newtype Broadword a = Broadword a deriving (Eq, Show, Storable)
