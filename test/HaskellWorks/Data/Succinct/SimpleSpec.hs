{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables              #-}

module HaskellWorks.Data.Succinct.SimpleSpec (spec) where

import           Data.Int
import           Data.Vector
import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module "HLint: ignore Redundant do" #-}

newtype Position_0_8  = Position_0_8  Position deriving (Eq,Show)
newtype Position_0_16 = Position_0_16 Position deriving (Eq,Show)
newtype Position_0_32 = Position_0_32 Position deriving (Eq,Show)
newtype Position_0_64 = Position_0_64 Position deriving (Eq,Show)

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

spec :: Spec
spec = describe "HaskellWorks.Data.SuccinctSpec" $ do
  it "rank1 for Simple (Vector Word8) and Simple (Vector Word64) should give same answer" $ property $
    \(Position_0_64 i) (a :: Word8) (b :: Word8) (c :: Word8) (d :: Word8)
                  (e :: Word8) (f :: Word8) (g :: Word8) (h :: Word8) ->
      let a64 = fromIntegral a :: Word64 in
      let b64 = fromIntegral b :: Word64 in
      let c64 = fromIntegral c :: Word64 in
      let d64 = fromIntegral d :: Word64 in
      let e64 = fromIntegral e :: Word64 in
      let f64 = fromIntegral f :: Word64 in
      let g64 = fromIntegral g :: Word64 in
      let h64 = fromIntegral h :: Word64 in
      let abcdefgh64 = (h64 .<. 56) .|. (g64 .<. 48) .|. (f64 .<. 40) .|. (e64 .<. 32) .|.
                       (d64 .<. 24) .|. (c64 .<. 16) .|. (b64 .<. 8 ) .|.  a64              in
      let vec16 = Simple (fromList [a, b, c, d, e, f, g, h] :: Vector Word8 )               in
      let vec64 = Simple (fromList [abcdefgh64]             :: Vector Word64)               in
      rank1 vec16 i == rank1 vec64 i
  it "rank1 for Simple (Vector Word16) and Simple (Vector Word64) should give same answer" $ property $
    \(Position_0_64 i) (a :: Word16) (b :: Word16) (c :: Word16) (d :: Word16) ->
      let a64 = fromIntegral a :: Word64 in
      let b64 = fromIntegral b :: Word64 in
      let c64 = fromIntegral c :: Word64 in
      let d64 = fromIntegral d :: Word64 in
      let abcd64 = (d64 .<. 48) .|. (c64 .<. 32) .|. (b64 .<. 16) .|. a64 in
      let vec16 = Simple (fromList [a, b, c, d] :: Vector Word16) in
      let vec64 = Simple (fromList [abcd64]     :: Vector Word64) in
      rank1 vec16 i == rank1 vec64 i
  it "rank1 for Simple (Vector Word32) and Simple (Vector Word64) should give same answer" $ property $
    \(Position_0_64 i) (a :: Word32) (b :: Word32) ->
      let a64 = fromIntegral a :: Word64 in
      let b64 = fromIntegral b :: Word64 in
      let ab64 = (b64 .<. 32) .|. a64 in
      let vec32 = Simple (fromList [a, b] :: Vector Word32) in
      let vec64 = Simple (fromList [ab64] :: Vector Word64) in
      rank1 vec32 i == rank1 vec64 i
