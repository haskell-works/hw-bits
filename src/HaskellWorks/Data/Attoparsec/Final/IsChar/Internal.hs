module HaskellWorks.Data.Attoparsec.Final.IsChar.Internal
    ( IsChar(..)
    ) where

class IsChar c where
  toChar :: c -> Char
