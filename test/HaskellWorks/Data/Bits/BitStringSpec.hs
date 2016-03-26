{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.BitStringSpec (spec) where

import qualified Data.Vector                      as DV
import           Data.Word
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitString
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.BitStringSpec" $ do
  it "fromBitString \"10000000 101\" :: Maybe [Word8]" $
    let w = fromBitString "10000000 101" :: Maybe [Bool] in
    w `shouldBe` Just [True, False, False, False, False, False, False, False, True, False, True]
  it "fromBitString \"10000000 101\" :: Maybe [Word8]"$
     let w = fromBitString "10000000 101" :: Maybe [Word8] in
    w `shouldBe` Just [1, 5]
  it "fromBitString \"11100100 10101111 1\" :: Maybe [Word8]" $
    let ws = fromBitString "11100100 10101111 1" :: Maybe [Word8] in
    ws `shouldBe` Just [39, 245, 1]
  it "fromBitString \"\" :: Maybe [Word8]" $
    let ws = fromBitString "" :: Maybe [Word8] in
    ws `shouldBe` Just []
  it "fromBitString \"10000000 101\" :: Maybe (DV.Vector Word8)" $
    let v = fromBitString "10000000 101" :: Maybe (DV.Vector Word8) in
    v `shouldBe` Just (DV.fromList [1, 5])
  it "fromBitString \"11100100 10101111 1\" :: Maybe (DV.Vector Word8)" $
    let v = fromBitString "11100100 10101111 1" :: Maybe (DV.Vector Word8) in
    v `shouldBe` Just (DV.fromList [39, 245, 1])
  it "fromBitString \"11100100 10101111 1\" :: Maybe (DV.Vector Word16)" $
    let v = fromBitString "11100100 10101111 1" :: Maybe (DV.Vector Word16) in
    v `shouldBe` Just (DV.fromList [39 + 62720, 1])
  it "fromBitString \"\" :: Maybe (DV.Vector Word8)" $
    let v = fromBitString "" :: Maybe (DV.Vector Word8) in
    v `shouldBe` Just (DV.fromList [])
  it "bitShow (8 :: Word8)" $
    let bs = toBitString (8 :: Word8) in
    bs `shouldBe` "00010000"
  it "bitShow (8 :: Word64)" $
    let bs = toBitString (8 :: Word64) in
    bs `shouldBe` "00010000 00000000 00000000 00000000 00000000 00000000 00000000 00000000"
  it "bitShow [0x0102040810204080 :: Word64]" $
    let bs = toBitString [0x0102040810204080 :: Word64] in
    bs `shouldBe` "00000001 00000010 00000100 00001000 00010000 00100000 01000000 10000000"
