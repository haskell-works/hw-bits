module HaskellWorks.Data.Attoparsec.Final.IsChar.Char
    ( IsChar(..)
    ) where

import HaskellWorks.Data.Attoparsec.Final.IsChar.Internal

instance IsChar Char where
  toChar = id
