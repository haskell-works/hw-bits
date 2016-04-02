{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Succinct.Cursor.InterestBits
  ( JsonInterestBits(..)
  , getJsonInterestBits
  , jsonBsToInterestBs
  , chunkup
  ) where

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
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Poppy512
import           HaskellWorks.Function

newtype JsonInterestBits a = JsonInterestBits a

getJsonInterestBits :: JsonInterestBits a -> a
getJsonInterestBits (JsonInterestBits a) = a

applyToMultipleOf :: (ByteString -> ByteString) -> ByteString -> Count -> ByteString
applyToMultipleOf f bs n = (f `applyN` fromIntegral ((n - (fromIntegral (BS.length bs) `mod` n)) `mod` n)) bs

chunkup :: ByteString -> [ByteString]
chunkup bs = if BS.length bs == 0
  then []
  else case BS.splitAt 1024 bs of
    (as, zs) -> as : chunkup zs

jsonBsToInterestBs :: ByteString -> ByteString
jsonBsToInterestBs textBS = BS.concat $ runListConduit [textBS] (blankJson =$= blankedJsonToInterestBits)

genInterest :: ByteString -> Maybe (Word8, ByteString)
genInterest bs  = if BS.null bs
  then Nothing
  else Just (BS.head bs, BS.tail bs)

instance FromByteString (JsonInterestBits (BitShown BS.ByteString)) where
  fromByteString textBS = JsonInterestBits (BitShown (BS.unfoldr genInterest (jsonBsToInterestBs textBS)))

instance FromByteString (JsonInterestBits (BitShown (DVS.Vector Word8))) where
  fromByteString textBS = JsonInterestBits (BitShown (DVS.unfoldr genInterest (jsonBsToInterestBs textBS)))

instance FromByteString (JsonInterestBits (BitShown (DVS.Vector Word16))) where
  fromByteString textBS = JsonInterestBits (BitShown (DVS.unsafeCast (DVS.unfoldr genInterest interestBS')))
    where interestBS' = applyToMultipleOf (`BS.snoc` 0) (jsonBsToInterestBs textBS) 2

instance FromByteString (JsonInterestBits (BitShown (DVS.Vector Word32))) where
  fromByteString textBS = JsonInterestBits (BitShown (DVS.unsafeCast (DVS.unfoldr genInterest interestBS')))
    where interestBS' = applyToMultipleOf (`BS.snoc` 0) (jsonBsToInterestBs textBS) 4

instance FromByteString (JsonInterestBits (BitShown (DVS.Vector Word64))) where
  fromByteString textBS = JsonInterestBits (BitShown (DVS.unsafeCast (DVS.unfoldr genInterest interestBS')))
    where interestBS' = applyToMultipleOf (`BS.snoc` 0) (jsonBsToInterestBs textBS) 8

instance FromByteString (JsonInterestBits Poppy512) where
  fromByteString = JsonInterestBits . makePoppy512 . bitShown . getJsonInterestBits . fromByteString
