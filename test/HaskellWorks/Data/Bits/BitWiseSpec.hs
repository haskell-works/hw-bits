{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.BitWiseSpec (spec) where

import qualified Data.Bits                      as B
import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           Test.Hspec
import           Test.QuickCheck

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

-- class BitLength v where
--   bitLength :: v -> Count

--   endPosition :: v -> Position
--   endPosition = Position . fromIntegral . getCount . bitLength

-- class Shift a where
--   (.<.) :: a -> Count -> a
--   (.>.) :: a -> Count -> a

-- class TestBit a where
--   (.?.) :: a -> Position -> Bool

-- class BitWise a where
--   (.&.) :: a -> a -> a
--   (.|.) :: a -> a -> a
--   (.^.) :: a -> a -> a
--   comp  :: a -> a
--   all0s :: a
--   all1s :: a

-- class PopCount v e where
--   popCount :: e -> v -> Count

-- class PopCount0 v where
--   popCount0 :: v -> Count

-- class PopCount1 v where
--   popCount1 :: v -> Count

-- --------------------------------------------------------------------------------
-- -- Functions

-- elemBitLength :: (VectorLike v e, BitLength e) => v e -> Count
-- elemBitLength v = bitLength (v !!! 0)

-- elemBitEnd :: (VectorLike v e, BitLength e) => v e -> Position
-- elemBitEnd v = endPosition (v !!! 0)


spec :: Spec
spec = describe "HaskellWorks.Data.SuccinctSpec" $ do
  describe "for popCount0" $ do
    it "for Word8 matches Data.Bits implementation" $ property $
      \(w :: Word8 ) -> popCount0 w == bitLength w - fromIntegral (B.popCount w)
    it "for Word16 matches Data.Bits implementation" $ property $
      \(w :: Word16) -> popCount0 w == bitLength w - fromIntegral (B.popCount w)
    it "for Word32 matches Data.Bits implementation" $ property $
      \(w :: Word32) -> popCount0 w == bitLength w - fromIntegral (B.popCount w)
    it "for Word64 matches Data.Bits implementation" $ property $
      \(w :: Word64) -> popCount0 w == bitLength w - fromIntegral (B.popCount w)
  describe "for popCount1" $ do
    it "for Word8 matches Data.Bits implementation" $ property $
      \(w :: Word8 ) -> popCount1 w == fromIntegral (B.popCount w)
    it "for Word16 matches Data.Bits implementation" $ property $
      \(w :: Word16) -> popCount1 w == fromIntegral (B.popCount w)
    it "for Word32 matches Data.Bits implementation" $ property $
      \(w :: Word32) -> popCount1 w == fromIntegral (B.popCount w)
    it "for Word64 matches Data.Bits implementation" $ property $
      \(w :: Word64) -> popCount1 w == fromIntegral (B.popCount w)
