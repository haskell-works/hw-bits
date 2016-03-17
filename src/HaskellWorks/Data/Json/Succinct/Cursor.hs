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
import           Data.Conduit
import           Data.String
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           Foreign.ForeignPtr
import           HaskellWorks.Data.Bits.FromBools
import           HaskellWorks.Data.Conduit.Json
import           HaskellWorks.Data.Json.Final.Tokenize.Internal
import           HaskellWorks.Data.Json.Succinct.Transform
import           HaskellWorks.Data.Json.Token
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens                  as BP
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Succinct.RankSelect.Simple
import           HaskellWorks.Data.Vector.VectorLike
import           Text.Parsec

class TreeCursor k where
  firstChild :: k -> k
  nextSibling :: k -> k
  parent :: k -> k
  depth :: k -> Count
  subtreeSize :: k -> Count

class HasJsonCursorType k where
  jsonCursorType :: k -> JsonCursorType

class FromByteString a where
  fromByteString :: ByteString -> JsonCursor BS.ByteString a

class FromForeignRegion a where
  fromForeignRegion :: (ForeignPtr Word8, Int, Int) -> JsonCursor BS.ByteString a

data JsonCursorType
  = JsonCursorArray
  | JsonCursorBool
  | JsonCursorNull
  | JsonCursorNumber
  | JsonCursorObject
  | JsonCursorString
  deriving (Eq, Show)

data JsonCursor t v = JsonCursor
  { cursorText     :: t
  , interests      :: Simple v
  , balancedParens :: SimpleBalancedParens v
  , cursorRank     :: Count
  }
  deriving (Eq, Show)

instance IsString (JsonCursor String [Bool]) where
  fromString :: String -> JsonCursor String [Bool]
  fromString s = JsonCursor
    { cursorText      = s
    , cursorRank      = 1
    , balancedParens  = SimpleBalancedParens (jsonToInterestBalancedParens [bs])
    , interests       = Simple interests'
    }
    where bs          = BSC.pack s :: BS.ByteString
          interests'  = jsonToInterestBits [bs]

instance (Monad m, DVS.Storable a) => Stream (DVS.Vector a) m a where
  uncons v | DVS.null v = return Nothing
           | otherwise = return (Just (DVS.head v, DVS.tail v))

(^^^) :: (a -> a) -> Count -> a -> a
(^^^) f n = foldl (.) id (replicate (fromIntegral n) f)

applyToMultipleOf :: (BS.ByteString -> BS.ByteString) -> BS.ByteString -> Count -> BS.ByteString
applyToMultipleOf f bs n = (f ^^^ ((n - (fromIntegral (BS.length bs) `mod` n)) `mod` n)) bs

jsonBsToInterestBs :: ByteString -> ByteString
jsonBsToInterestBs textBS = BS.concat $ runListConduit [textBS] (textToJsonToken =$= jsonToken2Markers =$= markerToByteString)

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

jsonCursorPos :: (Rank1 v, Select1 (Simple v), VectorLike s) => JsonCursor s v -> Position
jsonCursorPos k = toPosition (select1 ik (rank1 bpk (cursorRank k)) - 1)
  where ik  = interests k
        bpk = balancedParens k

jsonCursorElemAt :: (Rank1 v, Select1 (Simple v), VectorLike s) => JsonCursor s v -> Elem s
jsonCursorElemAt k = cursorText k !!! jsonCursorPos k

jsonTokenAt :: (Rank1 v, Select1 (Simple v)) => JsonCursor ByteString v -> JsonToken
jsonTokenAt k = case ABC.parse parseJsonToken (vDrop (toCount (jsonCursorPos k)) (cursorText k)) of
  ABC.Fail {} -> error "Failed to parse token in cursor"
  ABC.Done _ r -> r

instance HasJsonCursorType (JsonCursor String [Bool]) where
  jsonCursorType = jsonCursorType' . jsonCursorElemAt

instance HasJsonCursorType (JsonCursor BS.ByteString (DVS.Vector Word8)) where
  jsonCursorType = jsonCursorType' . chr . fromIntegral . jsonCursorElemAt

instance HasJsonCursorType (JsonCursor BS.ByteString (DVS.Vector Word16)) where
  jsonCursorType = jsonCursorType' . chr . fromIntegral . jsonCursorElemAt

instance HasJsonCursorType (JsonCursor BS.ByteString (DVS.Vector Word32)) where
  jsonCursorType = jsonCursorType' . chr . fromIntegral . jsonCursorElemAt

instance HasJsonCursorType (JsonCursor BS.ByteString (DVS.Vector Word64)) where
  jsonCursorType = jsonCursorType' . chr . fromIntegral . jsonCursorElemAt

instance TreeCursor (JsonCursor String [Bool]) where
  firstChild  k = k { cursorRank = BP.firstChild   (balancedParens k) (cursorRank k) }
  nextSibling k = k { cursorRank = BP.nextSibling  (balancedParens k) (cursorRank k) }
  parent      k = k { cursorRank = BP.parent (balancedParens k) (cursorRank k) }
  depth       k = BP.depth (balancedParens k) (cursorRank k)
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

instance TreeCursor (JsonCursor BS.ByteString (DVS.Vector Word8)) where
  firstChild  k = k { cursorRank = BP.firstChild   (balancedParens k) (cursorRank k) }
  nextSibling k = k { cursorRank = BP.nextSibling  (balancedParens k) (cursorRank k) }
  parent      k = k { cursorRank = BP.parent       (balancedParens k) (cursorRank k) }
  depth       k = BP.depth (balancedParens k) (cursorRank k)
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

instance TreeCursor (JsonCursor BS.ByteString (DVS.Vector Word16)) where
  firstChild  k = k { cursorRank = BP.firstChild   (balancedParens k) (cursorRank k) }
  nextSibling k = k { cursorRank = BP.nextSibling  (balancedParens k) (cursorRank k) }
  parent      k = k { cursorRank = BP.parent       (balancedParens k) (cursorRank k) }
  depth       k = BP.depth (balancedParens k) (cursorRank k)
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

instance TreeCursor (JsonCursor BS.ByteString (DVS.Vector Word32)) where
  firstChild  k = k { cursorRank = BP.firstChild   (balancedParens k) (cursorRank k) }
  nextSibling k = k { cursorRank = BP.nextSibling  (balancedParens k) (cursorRank k) }
  parent      k = k { cursorRank = BP.parent       (balancedParens k) (cursorRank k) }
  depth       k = BP.depth (balancedParens k) (cursorRank k)
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

instance TreeCursor (JsonCursor BS.ByteString (DVS.Vector Word64)) where
  firstChild  k = k { cursorRank = BP.firstChild   (balancedParens k) (cursorRank k) }
  nextSibling k = k { cursorRank = BP.nextSibling  (balancedParens k) (cursorRank k) }
  parent      k = k { cursorRank = BP.parent       (balancedParens k) (cursorRank k) }
  depth       k = BP.depth (balancedParens k) (cursorRank k)
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

instance FromByteString (DVS.Vector Word8) where
  fromByteString :: ByteString -> JsonCursor BS.ByteString (DVS.Vector Word8)
  fromByteString textBS = JsonCursor
    { cursorText     = textBS
    , interests      = Simple interestsV
    , balancedParens = SimpleBalancedParens (DVS.unfoldr fromBools (jsonToInterestBalancedParens [textBS]))
    , cursorRank     = 1
    }
    where interestsV      = DVS.unfoldr genInterest (jsonBsToInterestBs textBS) :: DVS.Vector Word8
          genInterest bs  = if BS.null bs
            then Nothing
            else Just (BS.head bs, BS.tail bs)

instance FromByteString (DVS.Vector Word16) where
  fromByteString :: ByteString -> JsonCursor BS.ByteString (DVS.Vector Word16)
  fromByteString textBS = JsonCursor
    { cursorText      = textBS
    , cursorRank      = 1
    , balancedParens  = SimpleBalancedParens bpV
    , interests       = Simple interestsV
    }
    where interestBS'      = applyToMultipleOf (`BS.snoc` 0) (jsonBsToInterestBs textBS) 2
          interestsV       = DVS.unsafeCast (DVS.unfoldr genInterest interestBS') :: DVS.Vector Word16
          genInterest bs   = if BS.null bs
            then Nothing
            else Just (BS.head bs, BS.tail bs)
          bpV             = DVS.unfoldr fromBools (jsonToInterestBalancedParens [textBS])

instance FromByteString (DVS.Vector Word32) where
  fromByteString :: ByteString -> JsonCursor BS.ByteString (DVS.Vector Word32)
  fromByteString textBS = JsonCursor
    { cursorText      = textBS
    , cursorRank      = 1
    , balancedParens  = SimpleBalancedParens bpV
    , interests       = Simple interestsV
    }
    where interestBS'      = applyToMultipleOf (`BS.snoc` 0) (jsonBsToInterestBs textBS) 4
          interestsV       = DVS.unsafeCast (DVS.unfoldr genInterest interestBS') :: DVS.Vector Word32
          genInterest bs   = if BS.null bs
            then Nothing
            else Just (BS.head bs, BS.tail bs)
          bpV             = DVS.unfoldr fromBools (jsonToInterestBalancedParens [textBS])

instance FromByteString (DVS.Vector Word64) where
  fromByteString :: ByteString -> JsonCursor BS.ByteString (DVS.Vector Word64)
  fromByteString textBS = JsonCursor
    { cursorText      = textBS
    , cursorRank      = 1
    , balancedParens  = SimpleBalancedParens bpV
    , interests       = Simple interestsV
    }
    where interestBS'      = applyToMultipleOf (`BS.snoc` 0) (jsonBsToInterestBs textBS) 8
          interestsV       = DVS.unsafeCast (DVS.unfoldr genInterest interestBS') :: DVS.Vector Word64
          genInterest bs   = if BS.null bs
            then Nothing
            else Just (BS.head bs, BS.tail bs)
          bpV             = DVS.unfoldr fromBools (jsonToInterestBalancedParens [textBS])

instance FromForeignRegion (DVS.Vector Word8) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (DVS.Vector Word16) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (DVS.Vector Word32) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance FromForeignRegion (DVS.Vector Word64) where
  fromForeignRegion (fptr, offset, size) = fromByteString (BSI.fromForeignPtr (castForeignPtr fptr) offset size)

instance IsString (JsonCursor BS.ByteString (DVS.Vector Word8)) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (DVS.Vector Word16)) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (DVS.Vector Word32)) where
  fromString = fromByteString . BSC.pack

instance IsString (JsonCursor BS.ByteString (DVS.Vector Word64)) where
  fromString = fromByteString . BSC.pack
