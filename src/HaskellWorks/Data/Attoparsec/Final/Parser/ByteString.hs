module HaskellWorks.Data.Attoparsec.Final.Parser.ByteString where

import qualified Data.Attoparsec.ByteString                         as ABS
import qualified Data.Attoparsec.ByteString.Char8                   as BC
import           Data.ByteString                                    (ByteString)
import           HaskellWorks.Data.Attoparsec.Final.IsChar
import           HaskellWorks.Data.Attoparsec.Final.Parser.Internal

instance Parser ByteString where
  satisfy = ABS.satisfy
  satisfyWith = ABS.satisfyWith
  satisfyChar = ABS.satisfyWith toChar
  string = ABS.string
  try = ABS.try
  char = BC.char
  (<?>) = (BC.<?>)
  rational = BC.rational
