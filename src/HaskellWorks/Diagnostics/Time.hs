{-# LANGUAGE BangPatterns #-}

module HaskellWorks.Diagnostics.Time where

import           System.CPUTime

measure :: a -> IO a
measure a = do
  start <- getCPUTime
  let !b = a
  end   <- getCPUTime
  print (end - start)
  return b
