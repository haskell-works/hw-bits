{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE UndecidableInstances #-}

module HaskellWorks.Data.Bits.BitString
  ( FromBitString(..)
  ) where

import qualified Data.Vector                     as DV
import qualified Data.Vector.Storable            as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitParse
import           HaskellWorks.Data.Bits.BitPrint
import           Text.ParserCombinators.Parsec

class FromBitString a where
  fromBitString :: String -> Maybe a

fromBitString' :: BitParse a => String -> Maybe a
fromBitString' = either (const Nothing) Just . parse bitParse0 "" . filter (/= ' ')

bitCharToBool :: Char -> Maybe Bool
bitCharToBool '1' = Just True
bitCharToBool '0' = Just False
bitCharToBool _   = Nothing

instance FromBitString Word8 where
  fromBitString = fromBitString'

instance FromBitString Word16 where
  fromBitString = fromBitString'

instance FromBitString Word32 where
  fromBitString = fromBitString'

instance FromBitString Word64 where
  fromBitString = fromBitString'

instance FromBitString [Word8] where
  fromBitString = fromBitString'

instance FromBitString [Word16] where
  fromBitString = fromBitString'

instance FromBitString [Word32] where
  fromBitString = fromBitString'

instance FromBitString [Word64] where
  fromBitString = fromBitString'

instance FromBitString (DV.Vector Word8) where
  fromBitString = fromBitString'

instance FromBitString (DV.Vector Word16) where
  fromBitString = fromBitString'

instance FromBitString (DV.Vector Word32) where
  fromBitString = fromBitString'

instance FromBitString (DV.Vector Word64) where
  fromBitString = fromBitString'

instance FromBitString (DVS.Vector Word8) where
  fromBitString = fromBitString'

instance FromBitString (DVS.Vector Word16) where
  fromBitString = fromBitString'

instance FromBitString (DVS.Vector Word32) where
  fromBitString = fromBitString'

instance FromBitString (DVS.Vector Word64) where
  fromBitString = fromBitString'

instance FromBitString [Bool] where
  fromBitString = sequence . fmap bitCharToBool . filter (/= ' ')
