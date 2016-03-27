{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Succinct.Cursor where

import qualified Data.Attoparsec.ByteString.Char8                           as ABC
import qualified Data.ByteString                                            as BS
import qualified Data.ByteString.Char8                                      as BSC
import           Data.ByteString.Internal                                   as BSI
import           Data.Char
import           Data.String
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           Foreign.ForeignPtr
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Json.Final.Tokenize.Internal
import           HaskellWorks.Data.Json.Succinct.Cursor.JsonBalancedParens
import           HaskellWorks.Data.Json.Succinct.Cursor.JsonCursorType
import           HaskellWorks.Data.Json.Succinct.Cursor.JsonInterestBits
import           HaskellWorks.Data.Json.Succinct.Transform
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens                  as BP
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Vector.VectorLike

class HasJsonCursorType k where
  jsonCursorType :: k -> JsonCursorType

class FromForeignRegion a where
  fromForeignRegion :: (ForeignPtr Word8, Int, Int) -> a

data JsonCursor t v w = JsonCursor
  { cursorText     :: t
  , interests      :: v
  , balancedParens :: w
  , cursorRank     :: Count
  }
  deriving (Eq, Show)

instance IsString (JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])) where
  fromString :: String -> JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
  fromString s = JsonCursor
    { cursorText      = s
    , cursorRank      = 1
    , balancedParens  = SimpleBalancedParens (jsonToInterestBalancedParens [bs])
    , interests       = BitShown interests'
    }
    where bs          = BSC.pack s :: BS.ByteString
          interests'  = jsonToInterestBits [bs]

jsonCursorType' :: Char -> JsonCursorType
jsonCursorType' c = case c of
  '[' -> JsonCursorArray
  't' -> JsonCursorBool
  'f' -> JsonCursorBool
  '0' -> JsonCursorNumber
  '1' -> JsonCursorNumber
  '2' -> JsonCursorNumber
  '3' -> JsonCursorNumber
  '4' -> JsonCursorNumber
  '5' -> JsonCursorNumber
  '6' -> JsonCursorNumber
  '7' -> JsonCursorNumber
  '8' -> JsonCursorNumber
  '9' -> JsonCursorNumber
  '+' -> JsonCursorNumber
  '-' -> JsonCursorNumber
  'n' -> JsonCursorNull
  '{' -> JsonCursorObject
  '"' -> JsonCursorString
  _   -> error "Invalid JsonCursor cursorRank"

jsonCursorPos :: (Rank1 w, Select1 v, VectorLike s) => JsonCursor s v w -> Position
jsonCursorPos k = toPosition (select1 ik (rank1 bpk (cursorRank k)) - 1)
  where ik  = interests k
        bpk = balancedParens k

jsonCursorElemAt :: (Rank1 w, Select1 v, VectorLike s) => JsonCursor s v w -> Elem s
jsonCursorElemAt k = cursorText k !!! jsonCursorPos k

jsonTokenAt :: (Rank1 w, Select1 v) => JsonCursor ByteString v w -> JsonToken
jsonTokenAt k = case ABC.parse parseJsonToken (vDrop (toCount (jsonCursorPos k)) (cursorText k)) of
  ABC.Fail    {}  -> error "Failed to parse token in cursor"
  ABC.Partial _   -> error "Failed to parse token in cursor"
  ABC.Done    _ r -> r

instance (Rank1 i, Select1 i, Rank1 b) => HasJsonCursorType (JsonCursor String i b) where
  jsonCursorType = jsonCursorType' . jsonCursorElemAt

instance (Rank1 i, Select1 i, Rank1 b) => HasJsonCursorType (JsonCursor BS.ByteString i b) where
  jsonCursorType = jsonCursorType' . chr . fromIntegral . jsonCursorElemAt

firstChild :: JsonCursor t v u -> JsonCursor t v u
firstChild k = k { cursorRank = BP.firstChild (balancedParens k) (cursorRank k) }

nextSibling :: BalancedParens u => JsonCursor t v u -> JsonCursor t v u
nextSibling k = k { cursorRank = BP.nextSibling (balancedParens k) (cursorRank k) }

parent :: BalancedParens u => JsonCursor t v u -> JsonCursor t v u
parent k = k { cursorRank = BP.parent (balancedParens k) (cursorRank k) }

depth :: (BalancedParens u, Rank1 u, Rank0 u) => JsonCursor t v u -> Count
depth k = BP.depth (balancedParens k) (cursorRank k)

subtreeSize :: BalancedParens u => JsonCursor t v u -> Count
subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

instance  (FromByteString (JsonInterestBits a), FromByteString (JsonBalancedParens b))
          => FromByteString (JsonCursor BS.ByteString a b) where
  fromByteString textBS = JsonCursor
    { cursorText      = textBS
    , interests       = getJsonInterestBits (fromByteString textBS)
    , balancedParens  = getJsonBalancedParens (fromByteString textBS)
    , cursorRank      = 1
    }

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32))) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))) where
  fromString = fromByteString . BSC.pack
