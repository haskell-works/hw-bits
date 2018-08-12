{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module HaskellWorks.Data.Bits.BitShow
  ( BitShow(..)
  , bitShow
  ) where

import Data.Word
import GHC.Exts
import HaskellWorks.Data.Bits.BitWise
import HaskellWorks.Data.Bits.Word

import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Vector          as DV
import qualified Data.Vector.Storable as DVS

-- | Shower of a value as a bit string
class BitShow a where
  -- | Show a value as a bit string
  bitShows :: a -> String -> String

instance BitShow Bool where
  bitShows a = ((if a then '1' else '0'):)

instance BitShow Word8 where
  bitShows w =
      (if w .?. 0 then ('1':) else ('0':))
    . (if w .?. 1 then ('1':) else ('0':))
    . (if w .?. 2 then ('1':) else ('0':))
    . (if w .?. 3 then ('1':) else ('0':))
    . (if w .?. 4 then ('1':) else ('0':))
    . (if w .?. 5 then ('1':) else ('0':))
    . (if w .?. 6 then ('1':) else ('0':))
    . (if w .?. 7 then ('1':) else ('0':))

instance BitShow Word16 where
  bitShows w = case leSplit w of (a, b) -> bitShows a . bitShows b

instance BitShow Word32 where
  bitShows w = case leSplit w of (a, b) -> bitShows a . bitShows b

instance BitShow Word64 where
  bitShows w = case leSplit w of (a, b) -> bitShows a . bitShows b

instance BitShow [Bool] where
  bitShows ws = ('\"':) . go (0 :: Int) ws . ('\"':)
    where go _ []     = id
          go _ [u]    = bitShows u
          go n (u:us) = bitShows u . maybePrependSeperatorat n . go (n + 1) us
          maybePrependSeperatorat n = if n `mod` 8 == 7 then (' ':) else id

instance BitShow BS.ByteString where
  bitShows bs | BS.length bs == 0 = id
  bitShows bs | BS.length bs == 1 = bitShows (BS.head bs)
  bitShows bs = bitShows (BS.head bs) . bitShows (BS.tail bs)

instance BitShow BSL.ByteString where
  bitShows bs | BSL.length bs == 0 = id
  bitShows bs | BSL.length bs == 1 = bitShows (BSL.head bs)
  bitShows bs = bitShows (BSL.head bs) . bitShows (BSL.tail bs)

instance BitShow [Word8] where
  bitShows []     = id
  bitShows [w]    = bitShows w
  bitShows (w:ws) = bitShows w . bitShows ws

instance BitShow [Word16] where
  bitShows []     = id
  bitShows [w]    = bitShows w
  bitShows (w:ws) = bitShows w . bitShows ws

instance BitShow [Word32] where
  bitShows []     = id
  bitShows [w]    = bitShows w
  bitShows (w:ws) = bitShows w . bitShows ws

instance BitShow [Word64] where
  bitShows []     = id
  bitShows [w]    = bitShows w
  bitShows (w:ws) = bitShows w . bitShows ws

instance BitShow (DV.Vector Word8) where
  bitShows = bitShows . toList

instance BitShow (DV.Vector Word16) where
  bitShows = bitShows . toList

instance BitShow (DV.Vector Word32) where
  bitShows = bitShows . toList

instance BitShow (DV.Vector Word64) where
  bitShows = bitShows . toList

instance BitShow (DVS.Vector Word8) where
  bitShows = bitShows . toList

instance BitShow (DVS.Vector Word16) where
  bitShows = bitShows . toList

instance BitShow (DVS.Vector Word32) where
  bitShows = bitShows . toList

instance BitShow (DVS.Vector Word64) where
  bitShows = bitShows . toList

bitShow :: BitShow a => a -> String
bitShow a = group8 0 (bitShows a "") ""
{-# INLINE bitShow #-}

group8 :: Int -> String -> (String -> String)
group8 i t = case t  of
  b:bs -> (if i > 0 && i `mod` 8 == 0 then (' ':) else id) . (b:) . group8 (i + 1) bs
  []   -> id
