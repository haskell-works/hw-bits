-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.FromBools
    ( FromBools(..)
    ) where

import           Data.Word
import           HaskellWorks.Data.Bits.BitWise

class FromBools a where
  fromBools :: [Bool] -> Maybe (a, [Bool])

instance FromBools Word8 where
  fromBools [] = Nothing
  fromBools xs = case splitAt 8 xs of
    (as, zs) -> case as ++ [False, False, False, False, False, False, False] of
      (a:b:c:d:e:f:g:h:_) ->
        Just (
          (if a then 0x01 else 0) .|.
          (if b then 0x02 else 0) .|.
          (if c then 0x04 else 0) .|.
          (if d then 0x08 else 0) .|.
          (if e then 0x10 else 0) .|.
          (if f then 0x20 else 0) .|.
          (if g then 0x40 else 0) .|.
          (if h then 0x80 else 0),
          zs)

instance FromBools Word16 where
  fromBools [] = Nothing
  fromBools xs = case splitAt 16 xs of
    (as, zs) -> case as ++ [False, False, False, False, False, False, False, False, False, False, False, False, False, False] of
      (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:_) ->
        Just (
          (if a then 0x0001 else 0) .|.
          (if b then 0x0002 else 0) .|.
          (if c then 0x0004 else 0) .|.
          (if d then 0x0008 else 0) .|.
          (if e then 0x0010 else 0) .|.
          (if f then 0x0020 else 0) .|.
          (if g then 0x0040 else 0) .|.
          (if h then 0x0080 else 0) .|.
          (if i then 0x0100 else 0) .|.
          (if j then 0x0200 else 0) .|.
          (if k then 0x0400 else 0) .|.
          (if l then 0x0800 else 0) .|.
          (if m then 0x1000 else 0) .|.
          (if n then 0x2000 else 0) .|.
          (if o then 0x4000 else 0) .|.
          (if p then 0x8000 else 0),
          zs)
