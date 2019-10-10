{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}

{-# LANGUAGE OverloadedLists     #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.FromBitTextByteStringSpec (spec) where

import Data.Word
import HaskellWorks.Data.Bits.FromBitTextByteString
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified Data.ByteString      as BS
import qualified Data.Vector.Storable as DVS

{-# ANN module ("HLint: Ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.FromBitTextByteStringSpec" $ do
  describe "For (DVS.Vector Word8)" $ do
    it "fromBitTextByteString (BS.unpack []) :: DVS.Vector Word8" . requireTest $ do
      let w = fromBitTextByteString (BS.pack []) :: DVS.Vector Word8
      w === DVS.fromList []
    it "fromBitTextByteString (BS.unpack \"0\") :: DVS.Vector Word8" . requireTest $ do
      let w = fromBitTextByteString "0" :: DVS.Vector Word8
      w === DVS.fromList [0x0]
    it "fromBitTextByteString (BS.unpack \"1\") :: DVS.Vector Word8" . requireTest $ do
      let w = fromBitTextByteString "1" :: DVS.Vector Word8
      w === DVS.fromList [0x1]
    it "fromBitTextByteString (BS.unpack \"11110000\") :: DVS.Vector Word8" . requireTest $ do
      let w = fromBitTextByteString "11110000" :: DVS.Vector Word8
      w === DVS.fromList [0x0f]
    it "fromBitTextByteString (BS.unpack \"111100000\") :: DVS.Vector Word8" . requireTest $ do
      let w = fromBitTextByteString "111100000" :: DVS.Vector Word8
      w === DVS.fromList [0x0f, 0x0]
    it "fromBitTextByteString (BS.unpack \"111100001\") :: DVS.Vector Word8" . requireTest $ do
      let w = fromBitTextByteString "111100001" :: DVS.Vector Word8
      w === DVS.fromList [0x0f, 0x1]
  describe "For (DVS.Vector Word16)" $ do
    it "fromBitTextByteString (BS.unpack []) :: DVS.Vector Word16" . requireTest $ do
      let w = fromBitTextByteString (BS.pack []) :: DVS.Vector Word16
      w === DVS.fromList []
    it "fromBitTextByteString (BS.unpack \"0\") :: DVS.Vector Word16" . requireTest $ do
      let w = fromBitTextByteString "0" :: DVS.Vector Word16
      w === DVS.fromList [0x0]
    it "fromBitTextByteString (BS.unpack \"1\") :: DVS.Vector Word16" . requireTest $ do
      let w = fromBitTextByteString "1" :: DVS.Vector Word16
      w === DVS.fromList [0x1]
    it "fromBitTextByteString (BS.unpack \"1111000011110000\") :: DVS.Vector Word16" . requireTest $ do
      let w = fromBitTextByteString "1111000011110000" :: DVS.Vector Word16
      w === DVS.fromList [0x0f0f]
    it "fromBitTextByteString (BS.unpack \"11110000111100000\") :: DVS.Vector Word16" . requireTest $ do
      let w = fromBitTextByteString "11110000111100000" :: DVS.Vector Word16
      w === DVS.fromList [0x0f0f, 0x0]
    it "fromBitTextByteString (BS.unpack \"11110000111100001\") :: DVS.Vector Word16" . requireTest $ do
      let w = fromBitTextByteString "11110000111100001" :: DVS.Vector Word16
      w === DVS.fromList [0x0f0f, 0x1]
  describe "For (DVS.Vector Word32)" $ do
    it "fromBitTextByteString (BS.unpack []) :: DVS.Vector Word32" . requireTest $ do
      let w = fromBitTextByteString (BS.pack []) :: DVS.Vector Word32
      w === DVS.fromList []
    it "fromBitTextByteString (BS.unpack \"0\") :: DVS.Vector Word32" . requireTest $ do
      let w = fromBitTextByteString "0" :: DVS.Vector Word32
      w === DVS.fromList [0x0]
    it "fromBitTextByteString (BS.unpack \"1\") :: DVS.Vector Word32" . requireTest $ do
      let w = fromBitTextByteString "1" :: DVS.Vector Word32
      w === DVS.fromList [0x1]
    it "fromBitTextByteString (BS.unpack \"11110000111100001111000011110000\") :: DVS.Vector Word32" . requireTest $ do
      let w = fromBitTextByteString "11110000111100001111000011110000" :: DVS.Vector Word32
      w === DVS.fromList [0x0f0f0f0f]
    it "fromBitTextByteString (BS.unpack \"111100001111000011110000111100000\") :: DVS.Vector Word32" . requireTest $ do
      let w = fromBitTextByteString "111100001111000011110000111100000" :: DVS.Vector Word32
      w === DVS.fromList [0x0f0f0f0f, 0x0]
    it "fromBitTextByteString (BS.unpack \"111100001111000011110000111100001\") :: DVS.Vector Word32" . requireTest $ do
      let w = fromBitTextByteString "111100001111000011110000111100001" :: DVS.Vector Word32
      w === DVS.fromList [0x0f0f0f0f, 0x1]
  describe "For (DVS.Vector Word64)" $ do
    it "fromBitTextByteString (BS.unpack []) :: DVS.Vector Word64" . requireTest $ do
      let w = fromBitTextByteString (BS.pack []) :: DVS.Vector Word64
      w === DVS.fromList []
    it "fromBitTextByteString (BS.unpack \"0\") :: DVS.Vector Word64" . requireTest $ do
      let w = fromBitTextByteString "0" :: DVS.Vector Word64
      w === DVS.fromList [0x0]
    it "fromBitTextByteString (BS.unpack \"1\") :: DVS.Vector Word64" . requireTest $ do
      let w = fromBitTextByteString "1" :: DVS.Vector Word64
      w === DVS.fromList [0x1]
    it "fromBitTextByteString (BS.unpack \"1111000011110000111100001111000011110000111100001111000011110000\") :: DVS.Vector Word64" . requireTest $ do
      let w = fromBitTextByteString "1111000011110000111100001111000011110000111100001111000011110000" :: DVS.Vector Word64
      w === DVS.fromList [0x0f0f0f0f0f0f0f0f]
    it "fromBitTextByteString (BS.unpack \"11110000111100001111000011110000111100001111000011110000111100000\") :: DVS.Vector Word64" . requireTest $ do
      let w = fromBitTextByteString "11110000111100001111000011110000111100001111000011110000111100000" :: DVS.Vector Word64
      w === DVS.fromList [0x0f0f0f0f0f0f0f0f, 0x0]
    it "fromBitTextByteString (BS.unpack \"11110000111100001111000011110000111100001111000011110000111100001\") :: DVS.Vector Word64" . requireTest $ do
      let w = fromBitTextByteString "11110000111100001111000011110000111100001111000011110000111100001" :: DVS.Vector Word64
      w === DVS.fromList [0x0f0f0f0f0f0f0f0f, 0x1]
