module HaskellWorks.Data.Succinct.BitString
  ( fromBitString
  ) where

import           HaskellWorks.Data.Succinct.BitParse
import           Text.ParserCombinators.Parsec

fromBitString :: BitParse a => String -> Maybe a
fromBitString = either (const Nothing) Just . parse bitParse0 ""
