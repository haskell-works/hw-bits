-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.FromBools
    ( FromBools(..)
    ) where

import           Data.Word
import           Debug.Trace
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Bits.FixedBitSize

class FromBools a where
  fromBools :: [Bool] -> Maybe (a, [Bool])

instance FromBools Word8 where
  fromBools [] = Nothing
  fromBools xs = go 0 0 xs
    where go _ w []        = Just (w, [])
          go n w (x:xs)
            | n < fixedBitSize w  = trace ("n = " ++ show n) $
                                    go (n + 1) (if x then w .|. (1 .<. n) else w) xs
            | n < 0               = error "Invalid index"
            | otherwise           = Just (w, x:xs)

instance FromBools Word16 where
  fromBools [] = Nothing
  fromBools xs = go 0 0 xs
    where go _ w []        = Just (w, [])
          go n w (x:xs)
            | n < fixedBitSize w  = trace ("n = " ++ show n) $
                                    go (n + 1) (if x then w .|. (1 .<. n) else w) xs
            | n < 0               = error "Invalid index"
            | otherwise           = Just (w, x:xs)

instance FromBools Word32 where
  fromBools [] = Nothing
  fromBools xs = go 0 0 xs
    where go _ w []        = Just (w, [])
          go n w (x:xs)
            | n < fixedBitSize w  = trace ("n = " ++ show n) $
                                    go (n + 1) (if x then w .|. (1 .<. n) else w) xs
            | n < 0               = error "Invalid index"
            | otherwise           = Just (w, x:xs)

instance FromBools Word64 where
  fromBools [] = Nothing
  fromBools xs = go 0 0 xs
    where go _ w []        = Just (w, [])
          go n w (x:xs)
            | n < fixedBitSize w  = trace ("n = " ++ show n) $
                                    go (n + 1) (if x then w .|. (1 .<. n) else w) xs
            | n < 0               = error "Invalid index"
            | otherwise           = Just (w, x:xs)
