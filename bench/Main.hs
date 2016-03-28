{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Criterion.Main
import qualified Data.ByteString                                     as BS
import qualified Data.Vector.Storable                                as DVS
import qualified Data.Vector.Storable                                as DVS
import           Data.Word                                           (Word64)
import           Data.Word
import           Foreign
import           HaskellWorks.Data.Bits.BitShown
import qualified HaskellWorks.Data.Bits.PopCount.PopCount1.Broadword as PC1BW
import qualified HaskellWorks.Data.Bits.PopCount.PopCount1.GHC       as PC1GHC
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Json.Succinct.Cursor
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic
import           System.IO.MMap

v :: DVS.Vector Word64
v = DVS.fromList (take 1000000 (cycle [maxBound, 0]))

setupEnv :: IO (DVS.Vector Word64)
setupEnv = return v

run :: String -> IO (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
run s = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr s ReadOnly Nothing
  let !cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
  return cursor

main :: IO ()
main = defaultMain
  [ env setupEnv (\bv -> bgroup "Rank"
    [ bench "Rank - Once"   (whnf (rank1    bv) 1)
    , bench "Select - Once" (whnf (select1  bv) 1)
    , bench "Rank - Many"   (nf   (map (getCount . rank1  bv)) [0, 1000..10000000])
    , bench "PopCnt1 Broadword - Once" (nf   (map (\n -> getCount (PC1BW.popCount1  (DVS.take n bv)))) [0, 1000..10000000])
    , bench "PopCnt1 GHC       - Once" (nf   (map (\n -> getCount (PC1GHC.popCount1 (DVS.take n bv)))) [0, 1000..10000000])
    , bench "Load 10 - Once" (whnfIO (run "part10.json"))
    , bench "Load 20 - Once" (whnfIO (run "part20.json"))
    , bench "Load 40 - Once" (whnfIO (run "part40.json"))
    , bench "Load 80 - Once" (whnfIO (run "part80.json"))
    ] )
  ]
