{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Conduit.JsonSpec (spec) where

import           Data.ByteString
import           Data.Conduit
import           Data.Int
import           HaskellWorks.Data.Bits.Conversion
import           HaskellWorks.Data.Conduit.Json
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.Conduit.Tokenize.Attoparsec
import           HaskellWorks.Data.Conduit.Tokenize.Attoparsec.Offset
import           HaskellWorks.Data.Json.Token
import           Test.Hspec

markerToBits :: [Int64] -> [Bool]
markerToBits ms = runListConduit ms (markerToByteString =$= byteStringToBits)

jsonToBits :: [ByteString] -> [Bool]
jsonToBits json = runListConduit json $
  textToJsonToken =$= jsonToken2Markers =$= markerToByteString =$= byteStringToBits

jsonToken2Markers2 :: [(ParseDelta Offset, JsonToken)] -> [Int64]
jsonToken2Markers2 json = runListConduit json jsonToken2Markers

spec :: Spec
spec = describe "Data.Conduit.Succinct.JsonSpec" $ do
  it "No markers should produce no bits" $
    markerToBits [] `shouldBe` []
  it "One marker < 8 should produce one byte with one bit set" $ do
    markerToBits [0] `shouldBe` stringToBits "10000000"
    markerToBits [1] `shouldBe` stringToBits "01000000"
    markerToBits [2] `shouldBe` stringToBits "00100000"
    markerToBits [3] `shouldBe` stringToBits "00010000"
    markerToBits [4] `shouldBe` stringToBits "00001000"
    markerToBits [5] `shouldBe` stringToBits "00000100"
    markerToBits [6] `shouldBe` stringToBits "00000010"
    markerToBits [7] `shouldBe` stringToBits "00000001"
  it "One 8 <= marker < 16 should produce one byte empty byte and another byte with one bit set" $ do
    markerToBits [ 8] `shouldBe` stringToBits "00000000 10000000"
    markerToBits [ 9] `shouldBe` stringToBits "00000000 01000000"
    markerToBits [10] `shouldBe` stringToBits "00000000 00100000"
    markerToBits [11] `shouldBe` stringToBits "00000000 00010000"
    markerToBits [12] `shouldBe` stringToBits "00000000 00001000"
    markerToBits [13] `shouldBe` stringToBits "00000000 00000100"
    markerToBits [14] `shouldBe` stringToBits "00000000 00000010"
    markerToBits [15] `shouldBe` stringToBits "00000000 00000001"
  it "All markers 0 .. 7 should produce one full byte" $
    markerToBits [0, 1, 2, 3, 4, 5, 6, 7] `shouldBe` stringToBits "11111111"
  it "All markers 0 .. 7 except 1 should produce one almost full byte" $ do
    markerToBits [1, 2, 3, 4, 5, 6, 7] `shouldBe` stringToBits "01111111"
    markerToBits [0, 2, 3, 4, 5, 6, 7] `shouldBe` stringToBits "10111111"
    markerToBits [0, 1, 3, 4, 5, 6, 7] `shouldBe` stringToBits "11011111"
    markerToBits [0, 1, 2, 4, 5, 6, 7] `shouldBe` stringToBits "11101111"
    markerToBits [0, 1, 2, 3, 5, 6, 7] `shouldBe` stringToBits "11110111"
    markerToBits [0, 1, 2, 3, 4, 6, 7] `shouldBe` stringToBits "11111011"
    markerToBits [0, 1, 2, 3, 4, 5, 7] `shouldBe` stringToBits "11111101"
    markerToBits [0, 1, 2, 3, 4, 5, 6] `shouldBe` stringToBits "11111110"
  it "All markers 0 .. 8 should produce one almost full byte and one near empty byte" $
    markerToBits [0, 1, 2, 3, 4, 5, 6, 7, 8] `shouldBe` stringToBits "11111111 10000000"
  it "Matching bits for bytes" $
    runListConduit [pack [0x80], pack [0xff], pack [0x01]] byteStringToBits `shouldBe`
      stringToBits "00000001 11111111 10000000"
  it "Every interesting token should produce a marker" $
    jsonToken2Markers2 [
      (ParseDelta (Offset 0) (Offset 1), JsonTokenBraceL),
      (ParseDelta (Offset 1) (Offset 2), JsonTokenBraceL),
      (ParseDelta (Offset 2) (Offset 3), JsonTokenBraceR),
      (ParseDelta (Offset 3) (Offset 4), JsonTokenBraceR)] `shouldBe` [0, 1]
  describe "When converting Json to tokens to markers to bits" $ do
    it "Empty Json should produce no bits" $
      jsonToBits [""] `shouldBe` []
    it "Spaces and newlines should produce no bits" $
      jsonToBits ["  \n \r \t "] `shouldBe` []
    it "number at beginning should produce one bit" $
      jsonToBits ["1234 "] `shouldBe` stringToBits "10000000"
    it "false at beginning should produce one bit" $
      jsonToBits ["false "] `shouldBe` stringToBits "10000000"
    it "true at beginning should produce one bit" $
      jsonToBits ["true "] `shouldBe` stringToBits "10000000"
    it "string at beginning should produce one bit" $
      jsonToBits ["\"hello\" "] `shouldBe` stringToBits "10000000"
    it "string at beginning should produce one bit" $
      jsonToBits ["\"\\\"\" "] `shouldBe` stringToBits "10000000"
    it "left brace at beginning should produce one bit" $
      jsonToBits ["{ "] `shouldBe` stringToBits "10000000"
    it "right brace at beginning should produce one bit" $
      jsonToBits ["} "] `shouldBe` stringToBits ""
    it "left bracket at beginning should produce one bit" $
      jsonToBits ["[ "] `shouldBe` stringToBits "10000000"
    it "right bracket at beginning should produce one bit" $
      jsonToBits ["] "] `shouldBe` stringToBits ""
    it "right bracket at beginning should produce one bit" $
      jsonToBits [": "] `shouldBe` stringToBits ""
    it "right bracket at beginning should produce one bit" $
      jsonToBits [", "] `shouldBe` stringToBits ""
    it "Four consecutive braces should produce four bits" $
      jsonToBits ["{{}}"] `shouldBe` stringToBits "11000000"
    it "Four spread out braces should produce four spread out bits" $
      jsonToBits [" { { } } "] `shouldBe` stringToBits "01010000"
