{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE UndecidableInstances #-}

module HaskellWorks.Data.Bits.BitRead
  ( BitRead(..)
  ) where

import qualified Data.ByteString                 as BS
import           Data.Maybe
import qualified Data.Vector                     as DV
import qualified Data.Vector.Storable            as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitParse
import           HaskellWorks.Data.String.Parse

-- | Bit string reader that produces a value of a type
class BitRead a where
  -- | Read a bit string into a value
  bitRead :: String -> Maybe a

bitRead' :: BitParse a => String -> Maybe a
bitRead' s = fst `fmap` listToMaybe (parse bitParse0 (filter (/= ' ') s))

bitCharToBool :: Char -> Maybe Bool
bitCharToBool '1' = Just True
bitCharToBool '0' = Just False
bitCharToBool _   = Nothing

instance BitRead Word8 where
  bitRead = bitRead'

instance BitRead Word16 where
  bitRead = bitRead'

instance BitRead Word32 where
  bitRead = bitRead'

instance BitRead Word64 where
  bitRead = bitRead'

instance BitRead BS.ByteString where
  bitRead = bitRead'

instance BitRead [Word8] where
  bitRead = bitRead'

instance BitRead [Word16] where
  bitRead = bitRead'

instance BitRead [Word32] where
  bitRead = bitRead'

instance BitRead [Word64] where
  bitRead = bitRead'

instance BitRead (DV.Vector Word8) where
  bitRead = bitRead'

instance BitRead (DV.Vector Word16) where
  bitRead = bitRead'

instance BitRead (DV.Vector Word32) where
  bitRead = bitRead'

instance BitRead (DV.Vector Word64) where
  bitRead = bitRead'

instance BitRead (DVS.Vector Word8) where
  bitRead = bitRead'

instance BitRead (DVS.Vector Word16) where
  bitRead = bitRead'

instance BitRead (DVS.Vector Word32) where
  bitRead = bitRead'

instance BitRead (DVS.Vector Word64) where
  bitRead = bitRead'

instance BitRead [Bool] where
  bitRead = sequence . fmap bitCharToBool . filter (/= ' ')
