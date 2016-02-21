{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables              #-}

module HaskellWorks.Data.SuccinctSpec (spec) where

import           Data.Int
import           Data.Word
import           HaskellWorks.Data.Succinct
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module "HLint: ignore Redundant do" #-}

newtype I64_0_8  = I64_0_8  Int64 deriving (Eq,Show)
newtype I64_0_16 = I64_0_16 Int64 deriving (Eq,Show)
newtype I64_0_32 = I64_0_32 Int64 deriving (Eq,Show)
newtype I64_0_64 = I64_0_64 Int64 deriving (Eq,Show)

instance Arbitrary I64_0_8 where
  arbitrary = do
     n <- choose (0, 8)
     return (I64_0_8 n)

instance Arbitrary I64_0_16 where
 arbitrary = do
    n <- choose (0, 16)
    return (I64_0_16 n)

instance Arbitrary I64_0_32 where
 arbitrary = do
    n <- choose (0, 32)
    return (I64_0_32 n)

instance Arbitrary I64_0_64 where
 arbitrary = do
    n <- choose (0, 64)
    return (I64_0_64 n)

spec :: Spec
spec = describe "HaskellWorks.Data.SuccinctSpec" $ do
  it "popCount 0 == 0 for all word sizes" $ do
    popCount (0 :: Word8 ) `shouldBe` 0
    popCount (0 :: Word16) `shouldBe` 0
    popCount (0 :: Word32) `shouldBe` 0
    popCount (0 :: Word64) `shouldBe` 0
  it "popCount -1 == bitLength for all word sizes" $ do
    popCount (-1 :: Word8 ) `shouldBe` 8
    popCount (-1 :: Word16) `shouldBe` 16
    popCount (-1 :: Word32) `shouldBe` 32
    popCount (-1 :: Word64) `shouldBe` 64
  it "bitRank for Word16 and Word64 should give same answer for bits 0-7" $ property $
    \(I64_0_8  i) (w :: Word8 ) -> bitRank w i == bitRank (fromIntegral w :: Word64) i
  it "bitRank for Word16 and Word64 should give same answer for bits 0-15" $ property $
    \(I64_0_16 i) (w :: Word16) -> bitRank w i == bitRank (fromIntegral w :: Word64) i
  it "bitRank for Word32 and Word64 should give same answer for bits 0-31" $ property $
    \(I64_0_32 i) (w :: Word32) -> bitRank w i == bitRank (fromIntegral w :: Word64) i
  it "bitRank for Word32 and Word64 should give same answer for bits 32-64" $ property $
    \(I64_0_32 i) (v :: Word32) (w :: Word32) ->
      let v64 = fromIntegral v :: Word64 in
      let w64 = fromIntegral w :: Word64 in
      bitRank v i + popCount w == bitRank ((v64 .<. 32) .|. w64) (i + 32)
  it "bitRank and bitSelect for Word64 form a galois connection" $ property $
    \(I64_0_32 i) (w :: Word32) -> 1 <= i && i <= popCount w ==>
      bitRank w (bitSelect w i) == i && bitSelect w (bitRank w i) <= i
