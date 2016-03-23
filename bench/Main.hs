module Main where

import           Criterion.Main
import qualified Data.Vector.Storable                                as DVS
import           Data.Word                                           (Word64)
import qualified HaskellWorks.Data.Bits.PopCount.PopCount1.Broadword as PC1BW
import qualified HaskellWorks.Data.Bits.PopCount.PopCount1.GHC       as PC1GHC
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
    , bench "PopCnt1 Broadword - Once" (nf   (map (\n -> getCount (PC1BW.popCount1  (DVS.take n bv)))) [0, 1000..10000000])
    , bench "PopCnt1 GHC       - Once" (nf   (map (\n -> getCount (PC1GHC.popCount1 (DVS.take n bv)))) [0, 1000..10000000])
    ] )
  ]
