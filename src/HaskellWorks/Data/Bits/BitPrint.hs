{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.BitPrint
  ( BitPrint(..)
  ) where

import Data.Word
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Word

class BitPrint a where
  bitPrint :: a -> String -> String

instance BitPrint Word8 where
  bitPrint w =
      (if w .?. 0 then ('1':) else ('0':))
    . (if w .?. 1 then ('1':) else ('0':))
    . (if w .?. 2 then ('1':) else ('0':))
    . (if w .?. 3 then ('1':) else ('0':))
    . (if w .?. 4 then ('1':) else ('0':))
    . (if w .?. 5 then ('1':) else ('0':))
    . (if w .?. 6 then ('1':) else ('0':))
    . (if w .?. 7 then ('1':) else ('0':))

instance BitPrint Word16 where
  bitPrint w = case leSplit w of (a, b) -> bitPrint a . (' ':) . bitPrint b

instance BitPrint Word32 where
  bitPrint w = case leSplit w of (a, b) -> bitPrint a . (' ':) . bitPrint b

instance BitPrint Word64 where
  bitPrint w = case leSplit w of (a, b) -> bitPrint a . (' ':) . bitPrint b
