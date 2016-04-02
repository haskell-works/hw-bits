{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Succinct.Cursor.InterestBits
  ( JsonInterestBits(..)
  , getJsonInterestBits
  , jsonBsToInterestBs
  ) where

import           Control.Applicative
import qualified Data.ByteString                                       as BS
import           Data.ByteString.Internal
import           Data.Conduit
import qualified Data.Vector.Storable                                  as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.Conduit.Json
import           HaskellWorks.Data.Conduit.Json.Blank
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512

newtype JsonInterestBits a = JsonInterestBits a

getJsonInterestBits :: JsonInterestBits a -> a
getJsonInterestBits (JsonInterestBits a) = a

jsonBsToInterestBs :: ByteString -> ByteString
jsonBsToInterestBs textBS = BS.concat $ runListConduit (blankJson =$= blankedJsonToInterestBits) [textBS]

genInterest :: ByteString -> Maybe (Word8, ByteString)
genInterest = BS.uncons

genInterestForever :: ByteString -> Maybe (Word8, ByteString)
genInterestForever bs = BS.uncons bs <|> Just (0, bs)

instance FromByteString (JsonInterestBits (BitShown BS.ByteString)) where
  fromByteString textBS = JsonInterestBits (BitShown (BS.unfoldr genInterest (jsonBsToInterestBs textBS)))

instance FromByteString (JsonInterestBits (BitShown (DVS.Vector Word8))) where
  fromByteString textBS = JsonInterestBits (BitShown (DVS.unfoldr genInterest (jsonBsToInterestBs textBS)))

instance FromByteString (JsonInterestBits (BitShown (DVS.Vector Word16))) where
  fromByteString textBS = JsonInterestBits (BitShown (DVS.unsafeCast (DVS.unfoldrN newLen genInterestForever interestBS)))
    where interestBS  = jsonBsToInterestBs textBS
          newLen      = (BS.length interestBS + 1) `div` 2 * 2

instance FromByteString (JsonInterestBits (BitShown (DVS.Vector Word32))) where
  fromByteString textBS = JsonInterestBits (BitShown (DVS.unsafeCast (DVS.unfoldrN newLen genInterestForever interestBS)))
    where interestBS  = jsonBsToInterestBs textBS
          newLen      = (BS.length interestBS + 3) `div` 4 * 4

instance FromByteString (JsonInterestBits (BitShown (DVS.Vector Word64))) where
  fromByteString textBS = JsonInterestBits (BitShown (DVS.unsafeCast (DVS.unfoldrN newLen genInterestForever interestBS)))
    where interestBS  = jsonBsToInterestBs textBS
          newLen      = (BS.length interestBS + 7) `div` 8 * 8

instance FromByteString (JsonInterestBits Poppy512) where
  fromByteString = JsonInterestBits . makePoppy512 . bitShown . getJsonInterestBits . fromByteString
