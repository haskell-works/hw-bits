{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE GADTs                #-}
{-# LANGUAGE KindSignatures       #-}
{-# LANGUAGE PolyKinds            #-}
{-# LANGUAGE RankNTypes           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeOperators        #-}

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

data SubWord64Vector (n :: Nat) = SubWord64Vector
    { swBuffer      :: !(DVS.Vector Word64)
    , swBufferLen   :: !Int
    } deriving (Eq, Show)

fromList :: Integer -> [Word64] -> (forall n. (KnownNat n, 1 <= n, n <= 64) => Maybe (SubWord64Vector n))
fromList n ws = case someNatVal n of
  Just (SomeNat m) | 1 <= n && n <= 64 ->
    Just SubWord64Vector
    { swBuffer    = DVS.fromList (packBits (fromIntegral (natVal m)) ws)
    , swBufferLen = fromIntegral (length ws)
    }
  _ -> Nothing

toList :: forall n. (KnownNat n, 1 <= n, n <= 64) => SubWord64Vector n -> [Word64]
toList v = unpackBits (swBufferLen v) (fromIntegral (natVal (Proxy :: Proxy n))) (DVS.toList (swBuffer v))
