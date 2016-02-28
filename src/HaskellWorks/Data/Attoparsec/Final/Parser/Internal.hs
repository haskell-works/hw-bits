module HaskellWorks.Data.Attoparsec.Final.Parser.Internal
    ( Parser(..)
    ) where

import qualified Data.Attoparsec.Types              as T
import           Data.MonoTraversable

class MonoTraversable t => Parser t where
  satisfy :: (Element t -> Bool) -> T.Parser t (Element t)
  satisfyWith :: (Element t -> a) -> (a -> Bool) -> T.Parser t a
  satisfyChar :: (Char -> Bool) -> T.Parser t Char
  string :: t -> T.Parser t t
  try :: T.Parser t a -> T.Parser t a
  char :: Char -> T.Parser t Char
  (<?>) :: T.Parser t Char -> String -> T.Parser t Char
  rational :: Fractional f => T.Parser t f
