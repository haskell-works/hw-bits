{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Succinct.Cursor.BalancedParens
  ( JsonBalancedParens(..)
  , getJsonBalancedParens
  ) where

import qualified Data.Vector.Storable                               as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.FromBools
import           HaskellWorks.Data.Conduit.Json
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.Json.Succinct.Cursor.BlankedJson
import           HaskellWorks.Data.Succinct.BalancedParens          as BP

newtype JsonBalancedParens a = JsonBalancedParens a

getJsonBalancedParens :: JsonBalancedParens a -> a
getJsonBalancedParens (JsonBalancedParens a) = a

instance FromBlankedJson (JsonBalancedParens (SimpleBalancedParens (DVS.Vector Word8))) where
  fromBlankedJson (BlankedJson bj) = JsonBalancedParens (SimpleBalancedParens (DVS.unfoldr fromBools (runListConduit blankedJsonToBalancedParens bj)))

instance FromBlankedJson (JsonBalancedParens (SimpleBalancedParens (DVS.Vector Word16))) where
  fromBlankedJson (BlankedJson bj) = JsonBalancedParens (SimpleBalancedParens (DVS.unfoldr fromBools (runListConduit blankedJsonToBalancedParens bj)))

instance FromBlankedJson (JsonBalancedParens (SimpleBalancedParens (DVS.Vector Word32))) where
  fromBlankedJson (BlankedJson bj) = JsonBalancedParens (SimpleBalancedParens (DVS.unfoldr fromBools (runListConduit blankedJsonToBalancedParens bj)))

instance FromBlankedJson (JsonBalancedParens (SimpleBalancedParens (DVS.Vector Word64))) where
  fromBlankedJson (BlankedJson bj) = JsonBalancedParens (SimpleBalancedParens (DVS.unfoldr fromBools (runListConduit blankedJsonToBalancedParens bj)))
