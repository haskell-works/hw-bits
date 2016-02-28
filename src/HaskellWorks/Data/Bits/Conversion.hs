{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module HaskellWorks.Data.Bits.Conversion
  ( AsBits(..)
  , FromBits(..)
  , bitsToString
  , bitsShows
  , fromBitsDiff
  , fromBitsDiffN
  , stringToBits
  )
  where

import Data.Bits
import Data.Word

bitsDiff' :: FiniteBits a => a -> Int -> Int -> [Bool] -> [Bool]
bitsDiff' a n len bs
  | n < len   = testBit a n : bitsDiff' a (n + 1) len bs
  | n == len  = bs
  | otherwise = error "Invalid index"

bits :: AsBits a => a -> [Bool]
bits a = bitsDiff a []

bitsShows' :: [Bool] -> ShowS
bitsShows' [] s = s
bitsShows' (True :bs) s = '1':bitsShows' bs s
bitsShows' (False:bs) s = '0':bitsShows' bs s

bitsShows :: AsBits a => a -> ShowS
bitsShows = bitsShows' . bits

bitsToString :: AsBits a => a -> String
bitsToString bs = bitsShows bs ""

-- unbits :: AsBits a => [Bool] -> (a, [Bool])
-- unbits a =  _uu


-- bitsUnshows :: AsBits a => String -> (a, String)
-- bitsUnshows = bitsShows' . bits
--
-- stringToBits :: AsBits a => String -> (a, String)
-- stringToBits as = _u

class AsBits a where
  bitsDiff :: a -> [Bool] -> [Bool]

class FromBits a where
  fromBits1 :: [Bool] -> (Maybe a, [Bool])

--------------------------------------------------------------------------------

instance AsBits Bool where
  bitsDiff = (:)

instance AsBits Word8 where
  bitsDiff a = bitsDiff' a 0 (finiteBitSize a)

instance AsBits a => AsBits [a] where
  bitsDiff [] = id
  bitsDiff (x:xs) = bitsDiff x . bitsDiff xs

instance FromBits Bool where
  fromBits1 [] = (Nothing, [])
  fromBits1 (b:bs) = (Just b, bs)

instance FromBits Word8 where
  fromBits1 (a:b:c:d:e:f:g:h:bs) = (,)
    (Just $ if a then 0x01 else 0 .|.
            if b then 0x02 else 0 .|.
            if c then 0x04 else 0 .|.
            if d then 0x08 else 0 .|.
            if e then 0x10 else 0 .|.
            if f then 0x20 else 0 .|.
            if g then 0x40 else 0 .|.
            if h then 0x80 else 0)
    bs
  fromBits1 bs = (Nothing, bs)

instance FromBits Word16 where
  fromBits1 (a:b:c:d:e:f:g:h:i:j:k:l:m:n:o:p:bs) = (,)
    (Just $ if a then 0x0001 else 0 .|.
            if b then 0x0002 else 0 .|.
            if c then 0x0004 else 0 .|.
            if d then 0x0008 else 0 .|.
            if e then 0x0010 else 0 .|.
            if f then 0x0020 else 0 .|.
            if g then 0x0040 else 0 .|.
            if h then 0x0080 else 0 .|.
            if i then 0x0100 else 0 .|.
            if j then 0x0200 else 0 .|.
            if k then 0x0400 else 0 .|.
            if l then 0x0800 else 0 .|.
            if m then 0x1000 else 0 .|.
            if n then 0x2000 else 0 .|.
            if o then 0x4000 else 0 .|.
            if p then 0x8000 else 0)
    bs
  fromBits1 bs = (Nothing, bs)

fromBitsDiff :: FromBits a => [Bool] -> ([a] -> [a], [Bool])
fromBitsDiff bs = case fromBits1 bs of
  (Nothing, rs) -> (id  ,              rs)
  (Just a , rs) -> case fromBitsDiff rs of
    (f, ss) -> ((a:) . f, ss)

fromBitsDiffN :: FromBits a => Int -> [Bool] -> ([a] -> [a], [Bool])
fromBitsDiffN n bs
  | n >  0    = case fromBits1 bs of
                  (Nothing, rs) -> (id  , rs)
                  (Just a , rs) -> case fromBitsDiffN (n - 1) rs of
                    (f, ss) -> ((a:) . f, ss)
  | n == 0    = (id, bs)
  | n <  0    = error "Invalid count"
  | null bs   = (id, [])
  | otherwise = error "Error"

stringToBits :: String -> [Bool]
stringToBits [] = []
stringToBits ('1' :xs) = True :stringToBits xs
stringToBits ('0' :xs) = False:stringToBits xs
stringToBits (' ' :xs) = stringToBits xs
stringToBits ('\n':xs) = stringToBits xs
stringToBits (_   :_ ) = error "Invalid bit"
