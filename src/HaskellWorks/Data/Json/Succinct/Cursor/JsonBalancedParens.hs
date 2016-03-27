{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Succinct.Cursor.JsonBalancedParens
  ( JsonBalancedParens(..)
  , getJsonBalancedParens
  ) where

import qualified Data.Vector.Storable                      as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.FromBools
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Json.Succinct.Transform
import           HaskellWorks.Data.Succinct.BalancedParens as BP

newtype JsonBalancedParens a = JsonBalancedParens a

getJsonBalancedParens :: JsonBalancedParens a -> a
getJsonBalancedParens (JsonBalancedParens a) = a

instance FromByteString (JsonBalancedParens (SimpleBalancedParens (DVS.Vector Word8))) where
  fromByteString textBS = JsonBalancedParens (SimpleBalancedParens (DVS.unfoldr fromBools (jsonToInterestBalancedParens [textBS])))

instance FromByteString (JsonBalancedParens (SimpleBalancedParens (DVS.Vector Word16))) where
  fromByteString textBS = JsonBalancedParens (SimpleBalancedParens (DVS.unfoldr fromBools (jsonToInterestBalancedParens [textBS])))

instance FromByteString (JsonBalancedParens (SimpleBalancedParens (DVS.Vector Word32))) where
  fromByteString textBS = JsonBalancedParens (SimpleBalancedParens (DVS.unfoldr fromBools (jsonToInterestBalancedParens [textBS])))

instance FromByteString (JsonBalancedParens (SimpleBalancedParens (DVS.Vector Word64))) where
  fromByteString textBS = JsonBalancedParens (SimpleBalancedParens (DVS.unfoldr fromBools (jsonToInterestBalancedParens [textBS])))
