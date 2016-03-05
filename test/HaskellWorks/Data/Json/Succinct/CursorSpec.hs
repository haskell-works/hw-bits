{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Succinct.CursorSpec(spec) where

import           Control.Monad
import           Control.Monad.IO.Class
import           HaskellWorks.Data.Conduit.Json
import           HaskellWorks.Data.Json.Succinct
import           HaskellWorks.Data.Json.Succinct.Cursor    as C
import           HaskellWorks.Data.Succinct.BalancedParens
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

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
      jsonCursorType (C.firstChild cursor) `shouldBe` JsonCursorNull
    it "cursor can navigate to second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String [Bool]
      jsonCursorType (C.nextSibling (C.firstChild cursor)) `shouldBe` JsonCursorObject
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String [Bool]
      jsonCursorType (C.firstChild (C.nextSibling (C.firstChild cursor))) `shouldBe` JsonCursorString
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String [Bool]
      liftIO $ print $ jsonCursorType $ cursor { cursorRank = 5 }
      liftIO $ print $                C.firstChild cursor
      liftIO $ print $ C.nextSibling (C.firstChild cursor)
      jsonCursorType (C.nextSibling (C.firstChild (C.nextSibling (C.firstChild cursor))))  `shouldBe` JsonCursorNumber
