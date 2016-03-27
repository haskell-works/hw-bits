module HaskellWorks.Data.Search
    ( binarySearch
    ) where

binarySearch :: (Ord a, Integral n) => a -> (n -> a) -> n -> n -> n
binarySearch w f p q = if p + 1 >= q
  then p
  else let m = p + q `div` 2 in
    if w <= f m
      then binarySearch w f p m
      else binarySearch w f m q
{-# INLINABLE binarySearch #-}
