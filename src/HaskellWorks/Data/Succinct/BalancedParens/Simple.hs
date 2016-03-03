{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module HaskellWorks.Data.Succinct.BalancedParens.Simple
  ( SimpleBalancedParens(..)
  , findOpen
  , findClose
  ) where

import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Internal
import           HaskellWorks.Data.Succinct.BalancedParens.Internal
import           Prelude                                            as P

newtype SimpleBalancedParens a = SimpleBalancedParens a
  deriving (BitLength, Eq, Show, TestBit, Rank0, Rank1, Select0, Select1)

closeAt :: TestBit a => a -> Position -> Bool
closeAt v p = v .?. p

openAt :: TestBit a => a -> Position -> Bool
openAt v p = not (v .?. p)

require :: Bool -> String -> a -> a
require p msg v = if p then v else error msg

findOpen' :: (BitLength a, TestBit a) => Count -> SimpleBalancedParens a -> Position -> Position
findOpen' c v p =
  require (0 <= p && p < endPosition v) "Out of bounds" $
  if v `openAt` p
    then if c == 0
      then p
      else findOpen' (c - 1) v (p - 1)
    else findOpen' (c + 1) v (p - 1)

findClose' :: (BitLength a, TestBit a) => Count -> SimpleBalancedParens a -> Position -> Position
findClose' c v p =
  require (0 <= p && p < endPosition v) "Out of bounds" $
  if v `closeAt` p
    then if c == 0
      then p
      else findClose' (c + 1) v (p + 1)
    else findClose' (c - 1) v (p + 1)

instance (BitLength a, TestBit a) => BalancedParens (SimpleBalancedParens a) where
  findOpen  v p = if v `openAt`  p then p else findOpen'  (Count 0) v (p - 1)
  findClose v p = if v `closeAt` p then p else findClose' (Count 0) v (p + 1)
  enclose       = findOpen' (Count 1)
