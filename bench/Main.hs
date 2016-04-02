{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Criterion.Main
import qualified Data.ByteString                                     as BS
import qualified Data.ByteString.Internal                            as BSI
import           Data.Conduit                                        (Conduit,
                                                                      (=$=))
import qualified Data.Vector.Storable                                as DVS
import qualified Data.Vector.Storable                                as DVS
import           Data.Word                                           (Word64)
import           Data.Word
import           Foreign
import           HaskellWorks.Data.Bits.BitShown
import qualified HaskellWorks.Data.Bits.PopCount.PopCount1.Broadword as PC1BW
import qualified HaskellWorks.Data.Bits.PopCount.PopCount1.GHC       as PC1GHC
import           HaskellWorks.Data.Conduit.Json
import           HaskellWorks.Data.Conduit.Json.Blank
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.FromByteString
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

setupEnvJson40 :: FilePath -> IO BS.ByteString
setupEnvJson40 filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filepath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

run1 :: BS.ByteString -> JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
run1 bs = fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))

runJ2IB :: BS.ByteString -> BS.ByteString
runJ2IB bs = BS.concat $ runListConduit [bs] (blankEscapedChars =$= blankStrings =$= blankNumbers =$= blankIdentifiers =$= blankedJsonToInterestBits)

runBlankIdentifiers :: BS.ByteString -> BS.ByteString
runBlankIdentifiers bs = BS.concat $ runListConduit [bs] blankIdentifiers

runCon :: Conduit i [] BS.ByteString -> i -> BS.ByteString
runCon con bs = BS.concat $ runListConduit [bs] con

jsonToInterestBits3 = blankEscapedChars =$= blankStrings =$= blankNumbers =$= blankIdentifiers =$= blankedJsonToInterestBits

runBlankedJsonToInterestBits :: BS.ByteString -> BS.ByteString
runBlankedJsonToInterestBits bs = BS.concat $ runListConduit [bs] blankedJsonToInterestBits

main :: IO ()
main = defaultMain
  [ env setupEnv $ \bv -> bgroup "Nothing" []
  -- , env setupEnv $ \bv -> bgroup "Rank"
  --   [ bench "Rank - Once"   (whnf (rank1    bv) 1)
  --   , bench "Select - Once" (whnf (select1  bv) 1)
  --   , bench "Rank - Many"   (nf   (map (getCount . rank1  bv)) [0, 1000..10000000])
  --   , bench "PopCnt1 Broadword - Once" (nf   (map (\n -> getCount (PC1BW.popCount1  (DVS.take n bv)))) [0, 1000..10000000])
  --   , bench "PopCnt1 GHC       - Once" (nf   (map (\n -> getCount (PC1GHC.popCount1 (DVS.take n bv)))) [0, 1000..10000000])
  --   ]
  , env (setupEnvJson40 "/Users/jky/Downloads/part40.json") $ \bs -> bgroup "Json40"
    [ bench "Run blankEscapedChars            "  (whnf (runCon blankEscapedChars          ) bs)
    , bench "Run blankStrings                 "  (whnf (runCon blankStrings               ) bs)
    , bench "Run blankNumbers                 "  (whnf (runCon blankNumbers               ) bs)
    , bench "Run blankIdentifiers             "  (whnf (runCon blankIdentifiers           ) bs)
    , bench "Run blankedJsonToInterestBits    "  (whnf (runCon blankedJsonToInterestBits  ) bs)
    , bench "Run jsonToInterestBits3          "  (whnf (runCon jsonToInterestBits3        ) bs)
    ]
  ]
