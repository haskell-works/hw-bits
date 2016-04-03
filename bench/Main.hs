{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad.Trans.Resource                        (MonadThrow)
import           Criterion.Main
import qualified Data.ByteString                                     as BS
import qualified Data.ByteString.Internal                            as BSI
import           Data.Conduit                                        (Conduit,
                                                                      (=$=))
import qualified Data.Vector.Storable                                as DVS
import           Data.Word
import           Foreign
import           HaskellWorks.Data.Bits.BitShown
import qualified HaskellWorks.Data.Bits.PopCount.PopCount1.Broadword as PC1BW
import qualified HaskellWorks.Data.Bits.PopCount.PopCount1.GHC       as PC1GHC
import           HaskellWorks.Data.Conduit.Json
import           HaskellWorks.Data.Conduit.Json.Blank
import           HaskellWorks.Data.Conduit.List
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Json.Succinct.Cursor
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic
import           System.IO.MMap

v :: DVS.Vector Word64
v = DVS.fromList (take 1000000 (cycle [maxBound, 0]))

setupEnv :: IO (DVS.Vector Word64)
setupEnv = return v

setupEnvJson :: FilePath -> IO BS.ByteString
setupEnvJson filepath = do
  (fptr :: ForeignPtr Word8, offset, size) <- mmapFileForeignPtr filepath ReadOnly Nothing
  let !bs = BSI.fromForeignPtr (castForeignPtr fptr) offset size
  return bs

loadJson :: BS.ByteString -> JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
loadJson bs = fromByteString bs :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))

runBlankIdentifiers :: BS.ByteString -> BS.ByteString
runBlankIdentifiers bs = BS.concat $ runListConduit blankIdentifiers [bs]

runCon :: Conduit i [] BS.ByteString -> i -> BS.ByteString
runCon con bs = BS.concat $ runListConduit con [bs]

jsonToInterestBits3 :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
jsonToInterestBits3 = blankJson =$= blankedJsonToInterestBits

jsonToInterestBitsOld :: MonadThrow m => Conduit BS.ByteString m BS.ByteString
jsonToInterestBitsOld = textToJsonToken =$= jsonToken2Markers =$= markerToByteString

runBlankedJsonToInterestBits :: BS.ByteString -> BS.ByteString
runBlankedJsonToInterestBits bs = BS.concat $ runListConduit blankedJsonToInterestBits [bs]

benchRankSelect :: [Benchmark]
benchRankSelect =
  [ env setupEnv $ \bv -> bgroup "Rank"
    [ bench "Rank - Once"   (whnf (rank1    bv) 1)
    , bench "Select - Once" (whnf (select1  bv) 1)
    , bench "Rank - Many"   (nf   (map (getCount . rank1  bv)) [0, 1000..10000000])
    , bench "PopCnt1 Broadword - Once" (nf   (map (\n -> getCount (PC1BW.popCount1  (DVS.take n bv)))) [0, 1000..10000000])
    , bench "PopCnt1 GHC       - Once" (nf   (map (\n -> getCount (PC1GHC.popCount1 (DVS.take n bv)))) [0, 1000..10000000])
    ]
  ]

benchRankJson40Conduits :: [Benchmark]
benchRankJson40Conduits =
  [ env (setupEnvJson "/Users/jky/Downloads/part40.json") $ \bs -> bgroup "Json40"
    [ bench "Run blankEscapedChars            "  (whnf (runCon blankEscapedChars          ) bs)
    , bench "Run blankStrings                 "  (whnf (runCon blankStrings               ) bs)
    , bench "Run blankNumbers                 "  (whnf (runCon blankNumbers               ) bs)
    , bench "Run blankIdentifiers             "  (whnf (runCon blankIdentifiers           ) bs)
    , bench "Run blankedJsonToInterestBits    "  (whnf (runCon blankedJsonToInterestBits  ) bs)
    , bench "Run jsonToInterestBits3          "  (whnf (runCon jsonToInterestBits3        ) bs)
    , bench "Run jsonToInterestBitsOld        "  (whnf (runCon jsonToInterestBitsOld      ) bs)
    , bench "loadJson                         "  (whnf  loadJson                            bs)
    ]
  ]

benchRankJson80Conduits :: [Benchmark]
benchRankJson80Conduits =
  [ env (setupEnvJson "/Users/jky/Downloads/part80.json") $ \bs -> bgroup "Json40"
    [ bench "loadJson" (whnf loadJson bs)
    ]
  ]

benchRankJsonBigConduits :: [Benchmark]
benchRankJsonBigConduits =
  [ env (setupEnvJson "/Users/jky/Downloads/78mb.json") $ \bs -> bgroup "JsonBig"
    [ bench "loadJson" (whnf loadJson bs)
    ]
  ]

main :: IO ()
main = defaultMain benchRankJson40Conduits
