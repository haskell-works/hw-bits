module HaskellWorks.Data.Succinct.BalancedParens
  ( BalancedParens(..)
  ) where

import HaskellWorks.Data.Succinct.Positioning

class BalancedParens v where
  findOpen :: v -> Position -> Position
  findClose :: v -> Position -> Position
  enclose :: v -> Position -> Position
