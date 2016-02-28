module HaskellWorks.Data.Succinct.BitString
  ( fromBitString
  , toBitString
  ) where

import           HaskellWorks.Data.Succinct.BitParse
import           HaskellWorks.Data.Succinct.BitPrint
import           Text.ParserCombinators.Parsec

fromBitString :: BitParse a => String -> Maybe a
fromBitString = either (const Nothing) Just . parse bitParse0 "" . filter (/= ' ')

toBitString :: BitPrint a => a -> String
toBitString a = bitPrint a ""
