{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Json.Succinct.CursorSpec(spec) where

import qualified Data.ByteString                        as BS
import           Data.String
import qualified Data.Vector.Storable                   as DVS
import           Data.Word
import           HaskellWorks.Data.Json.Succinct
import           HaskellWorks.Data.Json.Succinct.Cursor as C
import           HaskellWorks.Data.Positioning
import           System.IO.MMap
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

fc :: C.TreeCursor k => k -> k
fc = C.firstChild

ns :: C.TreeCursor k => k -> k
ns = C.nextSibling

pn :: C.TreeCursor k => k -> k
pn = C.parent

cd :: C.TreeCursor k => k -> Count
cd = C.depth

ss :: C.TreeCursor k => k -> Count
ss = C.subtreeSize

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.CursorSpec" $ do
  describe "Cursor for [Bool]" $ do
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
  genSpec "JsonCursor BS.ByteString (DVS.Vector Word8)"  (undefined :: JsonCursor BS.ByteString (DVS.Vector Word8))
  genSpec "JsonCursor BS.ByteString (DVS.Vector Word16)" (undefined :: JsonCursor BS.ByteString (DVS.Vector Word16))
  genSpec "JsonCursor BS.ByteString (DVS.Vector Word32)" (undefined :: JsonCursor BS.ByteString (DVS.Vector Word32))
  genSpec "JsonCursor BS.ByteString (DVS.Vector Word64)" (undefined :: JsonCursor BS.ByteString (DVS.Vector Word64))

genSpec :: forall t . (IsString t, HasJsonCursorType t, Show t) => String -> t -> SpecWith ()
genSpec t _ = do
  describe ("Cursor for (" ++ t ++ ")") $ do
    it "initialises to beginning of empty object" $ do
      let cursor = "{}" :: t
      print $ "--> " ++ (show cursor)
      jsonCursorType cursor `shouldBe` JsonCursorObject
    it "initialises to beginning of empty object preceded by spaces" $ do
      let cursor = " {}" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      jsonCursorType cursor `shouldBe` JsonCursorObject
    it "initialises to beginning of number" $ do
      let cursor = "1234" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      jsonCursorType cursor `shouldBe` JsonCursorNumber
    it "initialises to beginning of string" $ do
      let cursor = "\"Hello\"" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      jsonCursorType cursor `shouldBe` JsonCursorString
    it "initialises to beginning of array" $ do
      let cursor = "[]" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      jsonCursorType cursor `shouldBe` JsonCursorArray
    it "initialises to beginning of boolean true" $ do
      let cursor = "true" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      jsonCursorType cursor `shouldBe` JsonCursorBool
    it "initialises to beginning of boolean false" $ do
      let cursor = "false" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      jsonCursorType cursor `shouldBe` JsonCursorBool
    it "initialises to beginning of null" $ do
      let cursor = "null" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      jsonCursorType cursor `shouldBe` JsonCursorNull
    it "cursor can navigate to first child of array" $ do
      let cursor = "[null]" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      jsonCursorType (fc cursor) `shouldBe` JsonCursorNull
    it "cursor can navigate to second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      jsonCursorType ((ns . fc) cursor) `shouldBe` JsonCursorObject
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      jsonCursorType ((fc . ns . fc) cursor) `shouldBe` JsonCursorString
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      jsonCursorType ((ns . fc . ns . fc) cursor)  `shouldBe` JsonCursorNumber
    it "depth at top" $ do
      let cursor = "[null]" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      cd cursor `shouldBe` 1
    it "depth at first child of array" $ do
      let cursor = "[null]" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      cd (fc cursor) `shouldBe` 2
    it "depth at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      cd ((ns . fc) cursor) `shouldBe` 2
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      cd ((fc . ns . fc) cursor) `shouldBe` 3
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString (DVS.Vector Word8)
      cd ((ns . fc . ns . fc) cursor)  `shouldBe` 3
    it "can navigate down and forwards" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/Resources/sample.json" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString (DVS.Vector Word8)
      jsonCursorType                                                              cursor  `shouldBe` JsonCursorObject
      jsonCursorType ((                                                       fc) cursor) `shouldBe` JsonCursorString
      jsonCursorType ((                                                  ns . fc) cursor) `shouldBe` JsonCursorObject
      jsonCursorType ((                                             fc . ns . fc) cursor) `shouldBe` JsonCursorString
      jsonCursorType ((                                        ns . fc . ns . fc) cursor) `shouldBe` JsonCursorString
      jsonCursorType ((                                   ns . ns . fc . ns . fc) cursor) `shouldBe` JsonCursorString
      jsonCursorType ((                              ns . ns . ns . fc . ns . fc) cursor) `shouldBe` JsonCursorObject
      jsonCursorType ((                         fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` JsonCursorString
      jsonCursorType ((                    ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` JsonCursorString
      jsonCursorType ((               ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` JsonCursorString
      jsonCursorType ((          ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` JsonCursorString
      jsonCursorType ((     ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` JsonCursorString
      jsonCursorType ((ns . ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` JsonCursorNumber
    it "can navigate up" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/Resources/sample.json" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString (DVS.Vector Word8)
      (                                                        pn . fc) cursor `shouldBe`                               cursor
      (                                                   pn . ns . fc) cursor `shouldBe`                               cursor
      (                                              pn . fc . ns . fc) cursor `shouldBe` (                    ns . fc) cursor
      (                                         pn . ns . fc . ns . fc) cursor `shouldBe` (                    ns . fc) cursor
      (                                    pn . ns . ns . fc . ns . fc) cursor `shouldBe` (                    ns . fc) cursor
      (                               pn . ns . ns . ns . fc . ns . fc) cursor `shouldBe` (                    ns . fc) cursor
      (                          pn . fc . ns . ns . ns . fc . ns . fc) cursor `shouldBe` (ns . ns . ns . fc . ns . fc) cursor
      (                     pn . ns . fc . ns . ns . ns . fc . ns . fc) cursor `shouldBe` (ns . ns . ns . fc . ns . fc) cursor
      (                pn . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor `shouldBe` (ns . ns . ns . fc . ns . fc) cursor
      (           pn . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor `shouldBe` (ns . ns . ns . fc . ns . fc) cursor
      (      pn . ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor `shouldBe` (ns . ns . ns . fc . ns . fc) cursor
      ( pn . ns . ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor `shouldBe` (ns . ns . ns . fc . ns . fc) cursor
    it "can get subtree size" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/Resources/sample.json" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString (DVS.Vector Word8)
      ss                                                              cursor  `shouldBe` 45
      ss ((                                                       fc) cursor) `shouldBe` 1
      ss ((                                                  ns . fc) cursor) `shouldBe` 43
      ss ((                                             fc . ns . fc) cursor) `shouldBe` 1
      ss ((                                        ns . fc . ns . fc) cursor) `shouldBe` 1
      ss ((                                   ns . ns . fc . ns . fc) cursor) `shouldBe` 1
      ss ((                              ns . ns . ns . fc . ns . fc) cursor) `shouldBe` 9
      ss ((                         fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` 1
      ss ((                    ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` 1
      ss ((               ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` 1
      ss ((          ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` 1
      ss ((     ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` 1
      ss ((ns . ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` 1

