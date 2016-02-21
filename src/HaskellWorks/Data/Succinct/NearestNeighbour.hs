module HaskellWorks.Data.Succinct.NearestNeighbour
  ( bitPred
  , bitSucc
  ) where

import           HaskellWorks.Data.Succinct.Positioning
import           HaskellWorks.Data.Succinct.RankSelect

bitPred :: (BitRank v, BitSelect v) => v -> Position -> Position
bitPred v p = bitSelect v (bitRank v p - 1)

bitSucc :: (BitRank v, BitSelect v) => v -> Position -> Position
bitSucc v p = bitSelect v (bitRank v p + 1)
