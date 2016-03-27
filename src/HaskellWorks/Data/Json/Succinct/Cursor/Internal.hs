{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Succinct.Cursor.Internal
  ( JsonCursor(..)
  ) where

import qualified Data.ByteString                                           as BS
import qualified Data.ByteString.Char8                                     as BSC
import           Data.String
import qualified Data.Vector.Storable                                      as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Json.Succinct.Cursor.JsonBalancedParens
import           HaskellWorks.Data.Json.Succinct.Cursor.JsonInterestBits
import           HaskellWorks.Data.Json.Succinct.Transform
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens                 as BP

data JsonCursor t v w = JsonCursor
  { cursorText     :: t
  , interests      :: v
  , balancedParens :: w
  , cursorRank     :: Count
  }
  deriving (Eq, Show)

instance  (FromByteString (JsonInterestBits a), FromByteString (JsonBalancedParens b))
          => FromByteString (JsonCursor BS.ByteString a b) where
  fromByteString textBS = JsonCursor
    { cursorText      = textBS
    , interests       = getJsonInterestBits (fromByteString textBS)
    , balancedParens  = getJsonBalancedParens (fromByteString textBS)
    , cursorRank      = 1
    }

instance IsString (JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])) where
  fromString :: String -> JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
  fromString s = JsonCursor
    { cursorText      = s
    , cursorRank      = 1
    , balancedParens  = SimpleBalancedParens (jsonToInterestBalancedParens [bs])
    , interests       = BitShown interests'
    }
    where bs          = BSC.pack s :: BS.ByteString
          interests'  = jsonToInterestBits [bs]

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))) where
  fromString = fromByteString . BSC.pack
