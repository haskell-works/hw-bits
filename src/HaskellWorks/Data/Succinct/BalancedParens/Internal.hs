module HaskellWorks.Data.Succinct.BalancedParens.Internal
  ( BalancedParens(..)
  ) where

import HaskellWorks.Data.Positioning

class BalancedParens v where
  findOpen :: v -> Position -> Position
  findClose :: v -> Position -> Position
  enclose :: v -> Position -> Position
