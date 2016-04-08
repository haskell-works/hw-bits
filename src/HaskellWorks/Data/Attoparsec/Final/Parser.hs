module HaskellWorks.Data.Attoparsec.Final.Parser
  ( Parser(..)
  ) where

import qualified Data.Attoparsec.ByteString                as ABS
import qualified Data.Attoparsec.ByteString.Char8          as BC
import qualified Data.Attoparsec.Text                      as AT
import qualified Data.Attoparsec.Types                     as T
import           Data.ByteString                           (ByteString)
import           Data.MonoTraversable
import           Data.Text                                 (Text)
import           HaskellWorks.Data.Attoparsec.Final.IsChar

class MonoTraversable t => Parser t where
  satisfy :: (Element t -> Bool) -> T.Parser t (Element t)
  satisfyWith :: (Element t -> a) -> (a -> Bool) -> T.Parser t a
  satisfyChar :: (Char -> Bool) -> T.Parser t Char
  string :: t -> T.Parser t t
  try :: T.Parser t a -> T.Parser t a
  char :: Char -> T.Parser t Char
  (<?>) :: T.Parser t Char -> String -> T.Parser t Char
  rational :: Fractional f => T.Parser t f

instance Parser ByteString where
  satisfy = ABS.satisfy
  satisfyWith = ABS.satisfyWith
  satisfyChar = ABS.satisfyWith toChar
  string = ABS.string
  try = ABS.try
  char = BC.char
  (<?>) = (BC.<?>)
  rational = BC.rational

instance Parser Text where
  satisfy = AT.satisfy
  satisfyWith = AT.satisfyWith
  satisfyChar = AT.satisfyWith toChar
  string = AT.string
  try = AT.try
  char = AT.char
  (<?>) = (AT.<?>)
  rational = AT.rational
