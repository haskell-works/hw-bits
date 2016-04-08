module HaskellWorks.Data.Attoparsec.Final.IsChar
    ( IsChar(..)
    ) where

import qualified Data.ByteString.Internal as BI
import           Data.Word8

class IsChar c where
  toChar :: c -> Char

instance IsChar Word8 where
  toChar = BI.w2c

instance IsChar Char where
  toChar = id
