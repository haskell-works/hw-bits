module Main where

import           Criterion.Main
import qualified Data.ByteString                                     as BS

setupEnvBs :: Int -> IO BS.ByteString
setupEnvBs n = return $ BS.pack (take n (cycle [maxBound, 0]))

setupEnvBss :: Int -> Int -> IO [BS.ByteString]
setupEnvBss n k = setupEnvBs n >>= \v -> return (replicate k v)

benchIdentity :: [Benchmark]
benchIdentity =
  [ env (setupEnvBss 4060 19968) $ \_ -> bgroup "Rank"
    [ bench "Rechunk"   (whnf id "")
    ]
  ]

main :: IO ()
main = defaultMain benchIdentity
