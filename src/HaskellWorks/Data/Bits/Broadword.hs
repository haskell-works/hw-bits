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
  , h
  , l
  ) where

import         Data.Word
import         HaskellWorks.Data.Bits.BitWise
import         HaskellWorks.Data.Positioning

mu :: Count -> Word64
mu k = (maxBound :: Word64) `div` ((1 .<. fromIntegral ((1 :: Word64) .<. k)) + 1)

l :: Int -> Word64
l 2   = 0x5555555555555555
l 4   = 0x1111111111111111
l 8   = 0x0101010101010101
l 16  = 0x0001000100010001
l 32  = 0x0000000100000001
l 64  = 0x0000000000000001
l k   = error ("Invalid h k where k = " ++ show k)

h :: Int -> Word64
h 2   = 0xaaaaaaaaaaaaaaaa
h 4   = 0x8888888888888888
h 8   = 0x8080808080808080
h 16  = 0x8000800080008000
h 32  = 0x8000000080000000
h 64  = 0x8000000000000000
h k   = error ("Invalid h k where k = " ++ show k)
