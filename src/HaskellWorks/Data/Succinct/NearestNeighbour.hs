module HaskellWorks.Data.Succinct.NearestNeighbour
  ( bitPred
  , bitSucc
  ) where

import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect

bitPred :: (Rank1 v, Select1 v) => v -> Position -> Position
bitPred v p = select1 v (rank1 v p - 1)

bitSucc :: (Rank1 v, Select1 v) => v -> Position -> Position
bitSucc v p = select1 v (rank1 v p + 1)
