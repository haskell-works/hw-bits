{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Succinct.Cursor where

import qualified Data.Attoparsec.ByteString.Char8                           as ABC
import qualified Data.ByteString                                            as BS
import           Data.ByteString.Internal                                   as BSI
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           Foreign.ForeignPtr
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.FromByteString
import           HaskellWorks.Data.Json.Final.Tokenize.Internal
import           HaskellWorks.Data.Json.Succinct.Cursor.Internal
import           HaskellWorks.Data.Json.Succinct.Cursor.JsonCursorType
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens                  as BP
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Vector.VectorLike

class FromForeignRegion a where
  fromForeignRegion :: (ForeignPtr Word8, Int, Int) -> a

jsonTokenAt :: (Rank1 w, Select1 v) => JsonCursor ByteString v w -> JsonToken
jsonTokenAt k = case ABC.parse parseJsonToken (vDrop (toCount (jsonCursorPos k)) (cursorText k)) of
  ABC.Fail    {}  -> error "Failed to parse token in cursor"
  ABC.Partial _   -> error "Failed to parse token in cursor"
  ABC.Done    _ r -> r

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

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)
