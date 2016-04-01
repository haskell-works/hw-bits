{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Copyright: 2016 John Ky
-- License: MIT
--
-- Succinct operations.
module HaskellWorks.Data.Bits.BitShow
  ( BitShow(..)
  , bitShow
  ) where

import qualified Data.ByteString                as BS
import qualified Data.Vector                    as DV
import qualified Data.Vector.Storable           as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Word

class BitShow a where
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
  bitShows w = case leSplit w of (a, b) -> bitShows a . (' ':) . bitShows b

instance BitShow Word32 where
  bitShows w = case leSplit w of (a, b) -> bitShows a . (' ':) . bitShows b

instance BitShow Word64 where
  bitShows w = case leSplit w of (a, b) -> bitShows a . (' ':) . bitShows b

instance BitShow [Bool] where
  bitShows ws = ('\"':) . go (0 :: Int) ws . ('\"':)
    where go _ []     = id
          go _ [u]    = bitShows u
          go n (u:us) = bitShows u . maybePrependSeperatorat n . go (n + 1) us
          maybePrependSeperatorat n = if n `mod` 8 == 7 then (' ':) else id

instance BitShow BS.ByteString where
  bitShows bs | BS.length bs == 0 = id
  bitShows bs | BS.length bs == 1 = bitShows (BS.head bs)
  bitShows bs                     = bitShows (BS.head bs) . (' ':) . bitShows (BS.tail bs)

instance BitShow [Word8] where
  bitShows []     = id
  bitShows [w]    = bitShows w
  bitShows (w:ws) = bitShows w . (' ':) . bitShows ws

instance BitShow [Word16] where
  bitShows []     = id
  bitShows [w]    = bitShows w
  bitShows (w:ws) = bitShows w . (' ':) . bitShows ws

instance BitShow [Word32] where
  bitShows []     = id
  bitShows [w]    = bitShows w
  bitShows (w:ws) = bitShows w . (' ':) . bitShows ws

instance BitShow [Word64] where
  bitShows []     = id
  bitShows [w]    = bitShows w
  bitShows (w:ws) = bitShows w . (' ':) . bitShows ws

instance BitShow (DV.Vector Word8) where
  bitShows = bitShows . DV.toList

instance BitShow (DV.Vector Word16) where
  bitShows = bitShows . DV.toList

instance BitShow (DV.Vector Word32) where
  bitShows = bitShows . DV.toList

instance BitShow (DV.Vector Word64) where
  bitShows = bitShows . DV.toList

instance BitShow (DVS.Vector Word8) where
  bitShows = bitShows . DVS.toList

instance BitShow (DVS.Vector Word16) where
  bitShows = bitShows . DVS.toList

instance BitShow (DVS.Vector Word32) where
  bitShows = bitShows . DVS.toList

instance BitShow (DVS.Vector Word64) where
  bitShows = bitShows . DVS.toList

bitShow :: BitShow a => a -> String
bitShow a = bitShows a ""
