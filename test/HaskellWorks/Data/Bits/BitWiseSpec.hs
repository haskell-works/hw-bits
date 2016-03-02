{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables              #-}

module HaskellWorks.Data.Bits.BitWiseSpec (spec) where

import qualified Data.Bits as B
import           Data.Int
import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module "HLint: ignore Redundant do" #-}

newtype Position_0_8  = Position_0_8  Position deriving (Eq, Show)
newtype Position_0_16 = Position_0_16 Position deriving (Eq, Show)
newtype Position_0_32 = Position_0_32 Position deriving (Eq, Show)
newtype Position_0_64 = Position_0_64 Position deriving (Eq, Show)

instance Arbitrary Position_0_8 where
  arbitrary = do
     n <- choose (0, 8 :: Int64)
     return (Position_0_8 (Position n))

instance Arbitrary Position_0_16 where
 arbitrary = do
    n <- choose (0, 16 :: Int64)
    return (Position_0_16 (Position n))

instance Arbitrary Position_0_32 where
 arbitrary = do
    n <- choose (0, 32 :: Int64)
    return (Position_0_32 (Position n))

instance Arbitrary Position_0_64 where
 arbitrary = do
    n <- choose (0, 64 :: Int64)
    return (Position_0_64 (Position n))

newtype Count_0_8  = Count_0_8  Count deriving (Eq, Show)
newtype Count_0_16 = Count_0_16 Count deriving (Eq, Show)
newtype Count_0_32 = Count_0_32 Count deriving (Eq, Show)
newtype Count_0_64 = Count_0_64 Count deriving (Eq, Show)

instance Arbitrary Count_0_8 where
  arbitrary = do
     n <- choose (0, 8 :: Word64)
     return (Count_0_8 (Count n))

instance Arbitrary Count_0_16 where
 arbitrary = do
    n <- choose (0, 16 :: Word64)
    return (Count_0_16 (Count n))

instance Arbitrary Count_0_32 where
 arbitrary = do
    n <- choose (0, 32 :: Word64)
    return (Count_0_32 (Count n))

instance Arbitrary Count_0_64 where
 arbitrary = do
    n <- choose (0, 64 :: Word64)
    return (Count_0_64 (Count n))

spec :: Spec
spec = describe "HaskellWorks.Data.SuccinctSpec" $ do
  it "popCount1 for Word8 matches Data.Bits implementation" $ property $
    \(w :: Word8 ) -> popCount1 w == fromIntegral (B.popCount w)
  it "popCount1 for Word16 matches Data.Bits implementation" $ property $
    \(w :: Word16) -> popCount1 w == fromIntegral (B.popCount w)
  it "popCount1 for Word32 matches Data.Bits implementation" $ property $
    \(w :: Word32) -> popCount1 w == fromIntegral (B.popCount w)
  it "popCount1 for Word64 matches Data.Bits implementation" $ property $
    \(w :: Word64) -> popCount1 w == fromIntegral (B.popCount w)
  it "bitRank for Word16 and Word64 should give same answer for bits 0-7" $ property $
    \(Position_0_8  i) (w :: Word8 ) -> bitRank w i == bitRank (fromIntegral w :: Word64) i
  it "bitRank for Word16 and Word64 should give same answer for bits 0-15" $ property $
    \(Position_0_16 i) (w :: Word16) -> bitRank w i == bitRank (fromIntegral w :: Word64) i
  it "bitRank for Word32 and Word64 should give same answer for bits 0-31" $ property $
    \(Position_0_32 i) (w :: Word32) -> bitRank w i == bitRank (fromIntegral w :: Word64) i
  it "bitRank for Word32 and Word64 should give same answer for bits 32-64" $ property $
    \(Position_0_32 i) (v :: Word32) (w :: Word32) ->
      let v64 = fromIntegral v :: Word64 in
      let w64 = fromIntegral w :: Word64 in
      bitRank v i + popCount1 w == bitRank ((v64 .<. 32) .|. w64) (i + 32)
  it "bitRank and bitSelect for Word64 form a galois connection" $ property $
    \(Count_0_32 i) (w :: Word32) -> 1 <= i && i <= popCount1 w ==>
      bitRank w (bitSelect w i) == i && bitSelect w (bitRank w (fromIntegral i)) <= (fromIntegral i)
