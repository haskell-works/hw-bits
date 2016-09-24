{-# LANGUAGE DataKinds        #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures   #-}
{-# LANGUAGE RankNTypes       #-}
{-# LANGUAGE TypeOperators    #-}
{-# LANGUAGE GADTs            #-}
{-# LANGUAGE ScopedTypeVariables            #-}
{-# LANGUAGE PolyKinds            #-}

module HaskellWorks.Data.Bits.SubWord64Vector
  ( SubWord64Vector(..)
  , fromList
  , toList
  ) where

import           Data.Proxy
import qualified Data.Vector.Storable as DVS
import           Data.Word
import           GHC.TypeLits
import           HaskellWorks.Data.Bits.SubWord64Vector.Internal

import Data.Maybe (isJust)
import Data.Proxy (Proxy(Proxy))


data SubWord64Vector (n :: Nat) = SubWord64Vector
    { swBuffer      :: !(DVS.Vector Word64)
    , swSize        :: !Word
    , swBufferLen   :: !Int
    } deriving (Eq, Show)

f :: forall n . (KnownNat n, KnownNat (n+2)) => Proxy n -> Integer
f _ = natVal (Proxy :: Proxy n) + natVal (Proxy :: Proxy (n+2))

fromList :: forall n. (KnownNat n, 1 <= n, n <= 64) => [Word64] -> SubWord64Vector n
fromList ws =
  SubWord64Vector
  { swBuffer    = DVS.fromList (packBits (fromIntegral (natVal (Proxy :: Proxy n))) ws)
  , swBufferLen = fromIntegral (length ws)
  , swSize      = fromIntegral (natVal (Proxy :: Proxy n))
  }

toList :: (1 <= n, n <= 64) => SubWord64Vector n -> [Word64]
toList v = unpackBits (swBufferLen v) (fromIntegral (swSize v)) (DVS.toList (swBuffer v))
