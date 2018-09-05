module HaskellWorks.Data.Bits.Gen where

import Data.Word
import HaskellWorks.Data.Bits.BitString
import Hedgehog

import qualified Data.ByteString as BS
import qualified Hedgehog.Gen    as G

bytestring :: MonadGen m => Range Int -> m Word8 -> m BS.ByteString
bytestring r g = BS.pack <$> G.list r g

bitstring :: MonadGen m => Range Int -> m Word8 -> m BitString
bitstring r g = toBitString <$> bytestring r g
