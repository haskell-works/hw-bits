module HaskellWorks.Function
  ( applyN
  ) where

applyN :: (a -> a) -> Int -> a -> a
applyN f n = foldl (.) id (replicate (fromIntegral n) f)
