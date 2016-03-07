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
import           HaskellWorks.Data.Succinct.BalancedParens as BP
import           HaskellWorks.Data.Succinct.RankSelect
import           HaskellWorks.Data.VectorLike
import           System.IO.MMap
import           Test.Hspec

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
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor (DVS.Vector Word8) (DVS.Vector Word8)
      let k = cursor in print $ fromIntegral (select1 (interests k) (cursorRank k) - 1)
      let k = cursor in print $ select1 (interests k) (cursorRank k)
      let k = cursor in print $ select1 (DVS.head (getSimple (interests k))) (cursorRank k)
      print $ jsonCursorType cursor
