{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.Broadword
  ( mu
  ) where

import         Data.Word
import         HaskellWorks.Data.Bits.BitWise
import         HaskellWorks.Data.Positioning

mu :: Count -> Word64
mu k = (maxBound :: Word64) `div` ((1 .<. fromIntegral ((1 :: Word64) .<. k)) + 1)
