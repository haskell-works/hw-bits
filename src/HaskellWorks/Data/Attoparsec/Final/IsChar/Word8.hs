module HaskellWorks.Data.Attoparsec.Final.IsChar.Word8 where

import qualified Data.ByteString.Internal               as BI
import           Data.Word8
import           HaskellWorks.Data.Attoparsec.Final.IsChar.Internal

instance IsChar Word8 where
  toChar = BI.w2c
