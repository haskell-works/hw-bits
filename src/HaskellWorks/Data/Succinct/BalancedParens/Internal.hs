module HaskellWorks.Data.Succinct.BalancedParens.Internal
  ( BalancedParens(..)
  , firstChild
  , nextSibling
  , parent
  , depth
  , subtreeSize
  ) where

import HaskellWorks.Data.Positioning
import HaskellWorks.Data.Succinct.RankSelect.Internal

class BalancedParens v where
  findOpen :: v -> Position -> Position
  findClose :: v -> Position -> Position
  enclose :: v -> Position -> Position

firstChild :: v -> Position -> Position
firstChild _ = (+ 1)

nextSibling :: (BalancedParens v) => v -> Position -> Position
nextSibling v p = findClose v p + 1

parent :: (BalancedParens v) => v -> Position -> Position
parent = enclose

depth :: (BalancedParens v, Rank1 v) => v -> Position -> Count
depth = rank1

subtreeSize :: (BalancedParens v) => v -> Position -> Count
subtreeSize v p = toCount ((findClose v p - p + 1) `quot` 2)
