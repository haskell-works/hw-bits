module Main where

import           Criterion.Main
import qualified Data.Vector.Storable                               as DVS
import           Data.Word                                          (Word64)
import           HaskellWorks.Data.Bits.PopCount.PopCount1
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic

v :: DVS.Vector Word64
v = DVS.fromList (take 1000000 (cycle [maxBound, 0]))

setupEnv :: IO (DVS.Vector Word64)
setupEnv = return v

main :: IO ()
main = defaultMain
  [ env setupEnv (\bv -> bgroup "Rank"
    [ bench "Rank - Once"   (whnf (rank1    bv) 1)
    , bench "Select - Once" (whnf (select1  bv) 1)
    , bench "Rank - Many"   (nf   (map (getCount . rank1  bv)) [0, 1000..10000000])
    , bench "PopCnt - Once" (whnf popCount1 bv)
    ] )
  ]
