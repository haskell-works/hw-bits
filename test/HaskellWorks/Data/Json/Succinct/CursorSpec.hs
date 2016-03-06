{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module HaskellWorks.Data.Json.Succinct.CursorSpec(spec) where

import           Control.Monad
import           Control.Monad.IO.Class
import qualified Data.ByteString                           as BS
import           Data.ByteString.Internal                  as BSI
import           Data.Char
import           Data.Conduit
import qualified Data.Vector.Storable                      as DVS
import           Data.Word
import           Foreign.ForeignPtr
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Conduit.Json
import           HaskellWorks.Data.Json.Succinct
import           HaskellWorks.Data.Json.Succinct.Cursor    as C
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.BalancedParens
import           HaskellWorks.Data.Succinct.RankSelect
import           HaskellWorks.Data.VectorLike
import           System.IO.MMap
import           Test.Hspec
import           Text.Parsec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}

fc :: C.TreeCursor k => k -> k
fc = C.firstChild

ns :: C.TreeCursor k => k -> k
ns = C.nextSibling

cd :: C.TreeCursor k => k -> Count
cd = C.depth

ss :: C.TreeCursor k => k -> Count
ss = C.subtreeSize

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.CursorSpec" $ do
  describe "Cursor" $ do
    it "initialises to beginning of empty object" $ do
      let cursor = "{}" :: JsonCursor String [Bool]
      jsonCursorType cursor `shouldBe` JsonCursorObject
    it "initialises to beginning of empty object preceded by spaces" $ do
      let cursor = " {}" :: JsonCursor String [Bool]
      jsonCursorType cursor `shouldBe` JsonCursorObject
    it "initialises to beginning of number" $ do
      let cursor = "1234" :: JsonCursor String [Bool]
      jsonCursorType cursor `shouldBe` JsonCursorNumber
    it "initialises to beginning of string" $ do
      let cursor = "\"Hello\"" :: JsonCursor String [Bool]
      jsonCursorType cursor `shouldBe` JsonCursorString
    it "initialises to beginning of array" $ do
      let cursor = "[]" :: JsonCursor String [Bool]
      jsonCursorType cursor `shouldBe` JsonCursorArray
    it "initialises to beginning of boolean true" $ do
      let cursor = "true" :: JsonCursor String [Bool]
      jsonCursorType cursor `shouldBe` JsonCursorBool
    it "initialises to beginning of boolean false" $ do
      let cursor = "false" :: JsonCursor String [Bool]
      jsonCursorType cursor `shouldBe` JsonCursorBool
    it "initialises to beginning of null" $ do
      let cursor = "null" :: JsonCursor String [Bool]
      jsonCursorType cursor `shouldBe` JsonCursorNull
    it "cursor can navigate to first child of array" $ do
      let cursor = "[null]" :: JsonCursor String [Bool]
      jsonCursorType (fc cursor) `shouldBe` JsonCursorNull
    it "cursor can navigate to second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String [Bool]
      jsonCursorType ((ns . fc) cursor) `shouldBe` JsonCursorObject
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String [Bool]
      jsonCursorType ((fc . ns . fc) cursor) `shouldBe` JsonCursorString
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String [Bool]
      jsonCursorType ((ns . fc . ns . fc) cursor)  `shouldBe` JsonCursorNumber

    it "depth at top" $ do
      let cursor = "[null]" :: JsonCursor String [Bool]
      cd cursor `shouldBe` 1
    it "depth at first child of array" $ do
      let cursor = "[null]" :: JsonCursor String [Bool]
      cd (fc cursor) `shouldBe` 2
    it "depth at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String [Bool]
      cd ((ns . fc) cursor) `shouldBe` 2
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String [Bool]
      cd ((fc . ns . fc) cursor) `shouldBe` 3
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String [Bool]
      cd ((ns . fc . ns . fc) cursor)  `shouldBe` 3
    it "can memory map a json file" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/Resources/sample.json" ReadOnly Nothing
      putStrLn "Hello world"
      print (fptr, offset, size)
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor (DVS.Vector Word8) (DVS.Vector Word8)
      print cursor

class FromForeignRegion a where
  fromForeignRegion :: (ForeignPtr Word8, Int, Int) -> a

instance FromForeignRegion (JsonCursor (DVS.Vector Word8) (DVS.Vector Word8)) where
  fromForeignRegion :: (ForeignPtr Word8, Int, Int) -> JsonCursor (DVS.Vector Word8) (DVS.Vector Word8)
  fromForeignRegion (fptr, offset, size) = JsonCursor
    { cursorText     = DVS.unsafeFromForeignPtr (castForeignPtr fptr) offset size :: DVS.Vector Word8
    , interests      = Simple interestsV
    , balancedParens = SimpleBalancedParens bpV
    , cursorRank     = 1
    }
    where textBS          = BSI.fromForeignPtr (castForeignPtr fptr) offset size :: ByteString
          interestBS      = BS.concat $ runListConduit [textBS] (textToJsonToken =$= jsonToken2Markers =$= markerToByteString)
          interestsV      = DVS.unfoldr genInterest interestBS :: DVS.Vector Word8
          genInterest bs  = if BS.null bs
            then Nothing
            else Just (BS.head bs, BS.tail bs)
          bpV             = DVS.unfoldr fromBits (jsonToInterestBalancedParens [textBS])

-- bitsToWord8 :: Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Bool -> Word8
-- bitsToWord8 a b c d e f g h =

fromBits :: [Bool] -> Maybe (Word8, [Bool])
fromBits [] = Nothing
fromBits xs = case splitAt 8 xs of
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

instance (Monad m, DVS.Storable a) => Stream (DVS.Vector a) m a where
  uncons v | DVS.null v = return Nothing
           | otherwise = return (Just (DVS.head v, DVS.tail v))

isOpenBrace :: Word8 -> Bool
isOpenBrace w = w == 123

isOpenBracket :: Word8 -> Bool
isOpenBracket w = w == 91

isDoubleQuote :: Word8 -> Bool
isDoubleQuote w = w == 34

isSingleQuot :: Word8 -> Bool
isSingleQuot w = w == 44

isBackslash :: Word8 -> Bool
isBackslash w = w == 92

isDigit :: Word8 -> Bool
isDigit w = 48 <= w && w <= 57

isWhitespace :: Word8 -> Bool
isWhitespace w = w == 10 || w == 13 || w == 32



-- isOpenBrace = fromIntegral (ord '{') :: Word8

-- makeInterests' :: DVS.Vector Word8 -> Position -> [Position] -> [Position]
-- makeInterests' _ 0 ps = ps
-- makeInterests' v q ps = case v !!! qm of
--   '{' -> makeInterests' v qm (qm:ps)
--   '[' -> makeInterests' v qm (qm:ps)
--   '{' -> makeInterests' v qm (qm:ps)
--   '{' -> makeInterests' v qm (qm:ps)
--   where qm = q - 1


-- makeInterests :: DVS.Vector Word8 -> [Position]
-- makeInterests v = makeInterests' v (bitLength v)
