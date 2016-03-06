{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}

module HaskellWorks.Data.Json.Succinct.CursorSpec(spec) where

import           Control.Monad
import           Control.Monad.IO.Class
import           Data.Char
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
    , interests      = Simple DVS.empty
    , balancedParens = SimpleBalancedParens DVS.empty
    , cursorRank     = 1
    }

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
