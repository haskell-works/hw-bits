{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.BitStringSpec (spec) where

import HaskellWorks.Data.Bits.BitPatterns
import HaskellWorks.Data.Bits.BitString
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Hspec.Hedgehog
import Hedgehog
import Test.Hspec

import qualified HaskellWorks.Data.Bits.Gen as G
import qualified Hedgehog.Gen               as G
import qualified Hedgehog.Range             as R

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Bits.BitStringSpec" $ do
  describe "for comp" $ do
    it "double complement" $ requireProperty $ do
      bss <- forAll $ G.bitstring (R.linear 0 1024) (G.word8 R.constantBounded)
      comp (comp bss) === bss
  describe "for comp" $ do
    it "complement 1s" $ requireProperty $ do
      n <- forAll $ G.count (R.linear 0 1024)
      comp (takeBytes n all1s) === takeBytes n all0s
    it "complement 9s" $ requireProperty $ do
      n <- forAll $ G.count (R.linear 0 1024)
      comp (takeBytes n all0s) === takeBytes n all1s
  describe "for xor" $ do
    it "double complement" $ requireProperty $ do
      bss <- forAll $ G.bitstring (R.linear 0 1024) (G.word8 R.constantBounded)
      bss .^. bss === takeBytes (lengthBytes bss) all0s
  it "distributive law" $ requireProperty $ do
    n   <- forAll $ G.int (R.linear 0 1024)
    ass <- forAll $ G.bitstring (R.singleton n) (G.word8 R.constantBounded)
    bss <- forAll $ G.bitstring (R.singleton n) (G.word8 R.constantBounded)
    css <- forAll $ G.bitstring (R.singleton n) (G.word8 R.constantBounded)
    ass .&. (bss .|. css) === (ass .&. bss) .|. (ass .&. css)
  it "de morgans law 1" $ requireProperty $ do
    n   <- forAll $ G.int (R.linear 0 1024)
    ass <- forAll $ G.bitstring (R.singleton n) (G.word8 R.constantBounded)
    bss <- forAll $ G.bitstring (R.singleton n) (G.word8 R.constantBounded)
    comp (ass .&. bss) === comp ass .|. comp bss
  it "de morgans law 2" $ requireProperty $ do
    n   <- forAll $ G.int (R.linear 0 1024)
    ass <- forAll $ G.bitstring (R.singleton n) (G.word8 R.constantBounded)
    bss <- forAll $ G.bitstring (R.singleton n) (G.word8 R.constantBounded)
    comp (ass .|. bss) === comp ass .&. comp bss
