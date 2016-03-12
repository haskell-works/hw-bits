{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module HaskellWorks.Data.Json.Succinct.Cursor where

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
import           HaskellWorks.Data.Json.Succinct.Transform
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens                  as BP
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           HaskellWorks.Data.Succinct.RankSelect.Simple
import           Text.Parsec

class TreeCursor k where
  firstChild :: k -> k
  nextSibling :: k -> k
  parent :: k -> k
  depth :: k -> Count
  subtreeSize :: k -> Count

class HasJsonCursorType k where
  jsonCursorType :: k -> JsonCursorType

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

instance TreeCursor (JsonCursor String [Bool]) where
  firstChild  k = k { cursorRank = rank1 (balancedParens k) (BP.firstChild   (balancedParens k) (select1 (balancedParens k) (cursorRank k))) }
  nextSibling k = k { cursorRank = rank1 (balancedParens k) (BP.nextSibling  (balancedParens k) (select1 (balancedParens k) (cursorRank k))) }
  parent      k = k { cursorRank = BP.parent (balancedParens k) (cursorRank k) }
  depth       k = BP.depth (balancedParens k) (select1 (balancedParens k) (cursorRank k))
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

data JsonCursorType
  = JsonCursorArray
  | JsonCursorBool
  | JsonCursorNull
  | JsonCursorNumber
  | JsonCursorObject
  | JsonCursorString
  deriving (Eq, Show)

instance HasJsonCursorType (JsonCursor String [Bool]) where
  jsonCursorType k = case c of
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
    where c = cursorText k !! fromIntegral (select1 (interests k) (cursorRank k) - 1)

instance (Monad m, DVS.Storable a) => Stream (DVS.Vector a) m a where
  uncons v | DVS.null v = return Nothing
           | otherwise = return (Just (DVS.head v, DVS.tail v))

instance IsString (JsonCursor BS.ByteString (DVS.Vector Word8)) where
  fromString :: String -> JsonCursor BS.ByteString (DVS.Vector Word8)
  fromString s = JsonCursor
    { cursorText      = textBS
    , cursorRank      = 1
    , balancedParens  = SimpleBalancedParens bpV
    , interests       = Simple interestsV
    }
    where textBS          = BSC.pack s :: BS.ByteString
          interestBS      = BS.concat $ runListConduit [textBS] (textToJsonToken =$= jsonToken2Markers =$= markerToByteString)
          interestsV      = DVS.unfoldr genInterest interestBS :: DVS.Vector Word8
          genInterest bs  = if BS.null bs
            then Nothing
            else Just (BS.head bs, BS.tail bs)
          bpV             = DVS.unfoldr fromBools (jsonToInterestBalancedParens [textBS])

(^^^) :: (a -> a) -> Count -> a -> a
(^^^) f n = foldl (.) id (replicate (fromIntegral n) f)

applyToMultipleOf :: (BS.ByteString -> BS.ByteString) -> BS.ByteString -> Count -> BS.ByteString
applyToMultipleOf f bs n = (f ^^^ ((n - (fromIntegral (BS.length bs) `mod` n)) `mod` n)) bs

instance IsString (JsonCursor BS.ByteString (DVS.Vector Word16)) where
  fromString :: String -> JsonCursor BS.ByteString (DVS.Vector Word16)
  fromString s = JsonCursor
    { cursorText      = textBS
    , cursorRank      = 1
    , balancedParens  = SimpleBalancedParens bpV
    , interests       = Simple interestsV
    }
    where textBS            = BSC.pack s :: BS.ByteString
          interestBS        = BS.concat $ runListConduit [textBS] (textToJsonToken =$= jsonToken2Markers =$= markerToByteString)
          interestBS'       = applyToMultipleOf (`BS.snoc` fromIntegral (ord ' ')) interestBS 2
          interestsV        = DVS.unsafeCast (DVS.unfoldr genInterest interestBS') :: DVS.Vector Word16
          genInterest bs    = if BS.null bs
            then Nothing
            else Just (BS.head bs, BS.tail bs)
          bpV             = DVS.unfoldr fromBools (jsonToInterestBalancedParens [textBS])

instance IsString (JsonCursor BS.ByteString (DVS.Vector Word32)) where
  fromString :: String -> JsonCursor BS.ByteString (DVS.Vector Word32)
  fromString s = JsonCursor
    { cursorText      = textBS
    , cursorRank      = 1
    , balancedParens  = SimpleBalancedParens bpV
    , interests       = Simple interestsV
    }
    where textBS            = BSC.pack s :: BS.ByteString
          interestBS        = BS.concat $ runListConduit [textBS] (textToJsonToken =$= jsonToken2Markers =$= markerToByteString)
          interestBS'       = applyToMultipleOf (`BS.snoc` fromIntegral (ord ' ')) interestBS 4
          interestsV        = DVS.unsafeCast (DVS.unfoldr genInterest interestBS') :: DVS.Vector Word32
          genInterest bs    = if BS.null bs
            then Nothing
            else Just (BS.head bs, BS.tail bs)
          bpV             = DVS.unfoldr fromBools (jsonToInterestBalancedParens [textBS])

instance IsString (JsonCursor BS.ByteString (DVS.Vector Word64)) where
  fromString :: String -> JsonCursor BS.ByteString (DVS.Vector Word64)
  fromString s = JsonCursor
    { cursorText      = textBS
    , cursorRank      = 1
    , balancedParens  = SimpleBalancedParens bpV
    , interests       = Simple interestsV
    }
    where textBS            = BSC.pack s :: BS.ByteString
          interestBS        = BS.concat $ runListConduit [textBS] (textToJsonToken =$= jsonToken2Markers =$= markerToByteString)
          interestBS'       = applyToMultipleOf (`BS.snoc` fromIntegral (ord ' ')) interestBS 8
          interestsV        = DVS.unsafeCast (DVS.unfoldr genInterest interestBS') :: DVS.Vector Word64
          genInterest bs    = if BS.null bs
            then Nothing
            else Just (BS.head bs, BS.tail bs)
          bpV             = DVS.unfoldr fromBools (jsonToInterestBalancedParens [textBS])

jsonCursorType' :: Word8 -> JsonCursorType
jsonCursorType' c = case c of
  91  {- [ -} -> JsonCursorArray
  116 {- t -} -> JsonCursorBool
  102 {- f -} -> JsonCursorBool
  48  {- 0 -} -> JsonCursorNumber
  49  {- 1 -} -> JsonCursorNumber
  50  {- 2 -} -> JsonCursorNumber
  51  {- 3 -} -> JsonCursorNumber
  52  {- 4 -} -> JsonCursorNumber
  53  {- 5 -} -> JsonCursorNumber
  54  {- 6 -} -> JsonCursorNumber
  55  {- 7 -} -> JsonCursorNumber
  56  {- 8 -} -> JsonCursorNumber
  57  {- 9 -} -> JsonCursorNumber
  43  {- + -} -> JsonCursorNumber
  45  {- - -} -> JsonCursorNumber
  110 {- n -} -> JsonCursorNull
  123 {- { -} -> JsonCursorObject
  34  {- " -} -> JsonCursorString
  _   -> error "Invalid JsonCursor cursorRank"

instance HasJsonCursorType (JsonCursor BS.ByteString (DVS.Vector Word8)) where
  jsonCursorType k = jsonCursorType' c
    where c   = cursorText k `BS.index` i
          i   = fromIntegral (select1 ik (rank1 bpk (cursorRank k)) - 1)
          ik  = interests k
          bpk = balancedParens k

instance HasJsonCursorType (JsonCursor BS.ByteString (DVS.Vector Word16)) where
  jsonCursorType k = jsonCursorType' c
    where c   = cursorText k `BS.index` i
          i   = fromIntegral (select1 ik (rank1 bpk (cursorRank k)) - 1)
          ik  = interests k
          bpk = balancedParens k

instance HasJsonCursorType (JsonCursor BS.ByteString (DVS.Vector Word32)) where
  jsonCursorType k = jsonCursorType' c
    where c   = cursorText k `BS.index` i
          i   = fromIntegral (select1 ik (rank1 bpk (cursorRank k)) - 1)
          ik  = interests k
          bpk = balancedParens k

instance HasJsonCursorType (JsonCursor BS.ByteString (DVS.Vector Word64)) where
  jsonCursorType k = jsonCursorType' c
    where c   = cursorText k `BS.index` i
          i   = fromIntegral (select1 ik (rank1 bpk (cursorRank k)) - 1)
          ik  = interests k
          bpk = balancedParens k

instance TreeCursor (JsonCursor BS.ByteString (DVS.Vector Word8)) where
  firstChild  k = k { cursorRank = BP.firstChild   (balancedParens k) (cursorRank k) }
  nextSibling k = k { cursorRank = BP.nextSibling  (balancedParens k) (cursorRank k) }
  parent      k = k { cursorRank = BP.parent       (balancedParens k) (cursorRank k) }
  depth       k = BP.depth (balancedParens k) (cursorRank k)
  subtreeSize k = BP.subtreeSize (balancedParens k) (cursorRank k)

class FromForeignRegion a where
  fromForeignRegion :: (ForeignPtr Word8, Int, Int) -> a

instance FromForeignRegion (JsonCursor BS.ByteString (DVS.Vector Word8)) where
  fromForeignRegion :: (ForeignPtr Word8, Int, Int) -> JsonCursor BS.ByteString (DVS.Vector Word8)
  fromForeignRegion (fptr, offset, size) = JsonCursor
    { cursorText     = textBS
    , interests      = Simple interestsV
    , balancedParens = SimpleBalancedParens (DVS.unfoldr fromBools (jsonToInterestBalancedParens [textBS]))
    , cursorRank     = 1
    }
    where textBS          = BSI.fromForeignPtr (castForeignPtr fptr) offset size :: ByteString
          interestBS      = BS.concat $ runListConduit [textBS] (textToJsonToken =$= jsonToken2Markers =$= markerToByteString)
          interestsV      = DVS.unfoldr genInterest interestBS :: DVS.Vector Word8
          genInterest bs  = if BS.null bs
            then Nothing
            else Just (BS.head bs, BS.tail bs)
