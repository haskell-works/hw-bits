module HaskellWorks.Data.Attoparsec.Final.Parser.Text where

import qualified Data.Attoparsec.Text                               as AT
import           Data.Text                                          (Text)
import           HaskellWorks.Data.Attoparsec.Final.IsChar
import           HaskellWorks.Data.Attoparsec.Final.Parser.Internal

instance Parser Text where
  satisfy = AT.satisfy
  satisfyWith = AT.satisfyWith
  satisfyChar = AT.satisfyWith toChar
  string = AT.string
  try = AT.try
  char = AT.char
  (<?>) = (AT.<?>)
  rational = AT.rational
