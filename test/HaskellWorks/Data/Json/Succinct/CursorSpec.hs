{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Json.Succinct.CursorSpec(spec) where

import qualified Data.ByteString                                          as BS
import           Data.String
import qualified Data.Vector.Storable                                     as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitLength
import           HaskellWorks.Data.Bits.BitPrint
import           HaskellWorks.Data.Bits.BitString
import           HaskellWorks.Data.Bits.BitWise
import           HaskellWorks.Data.Json.Succinct.Cursor                   as C
import           HaskellWorks.Data.Positioning
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           System.IO.MMap
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

fc :: JsonCursor t v -> JsonCursor t v
fc = C.firstChild

ns :: (BitLength v, TestBit v, BitPrint v) => JsonCursor t v -> JsonCursor t v
ns = C.nextSibling

pn :: (BitLength v, TestBit v, BitPrint v) => JsonCursor t v -> JsonCursor t v
pn = C.parent

cd :: (BitLength v, TestBit v, Rank1 v, Rank0 v, BitPrint v) => JsonCursor t v -> Count
cd = C.depth

ss :: (BitLength v, TestBit v, BitPrint v) => JsonCursor t v -> Count
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
  genSpec "DVS.Vector Word8"  (undefined :: DVS.Vector Word8)
  genSpec "DVS.Vector Word16" (undefined :: DVS.Vector Word16)
  genSpec "DVS.Vector Word32" (undefined :: DVS.Vector Word32)
  genSpec "DVS.Vector Word64" (undefined :: DVS.Vector Word64)
  it "Loads same Json consistentally from different backing vectors" $ do
    let cursor8   = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: JsonCursor BS.ByteString (DVS.Vector Word8)
    let cursor16  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: JsonCursor BS.ByteString (DVS.Vector Word16)
    let cursor32  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: JsonCursor BS.ByteString (DVS.Vector Word32)
    let cursor64  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: JsonCursor BS.ByteString (DVS.Vector Word64)
    cursorText cursor8 `shouldBe` cursorText cursor16
    cursorText cursor8 `shouldBe` cursorText cursor32
    cursorText cursor8 `shouldBe` cursorText cursor64
    let ic8   = toBitString $ interests cursor8
    let ic16  = toBitString $ interests cursor16
    let ic32  = toBitString $ interests cursor32
    let ic64  = toBitString $ interests cursor64
    ic16 `shouldBeginWith` ic8
    ic32 `shouldBeginWith` ic16
    ic64 `shouldBeginWith` ic32

shouldBeginWith :: (Eq a, Show a) => [a] -> [a] -> IO ()
shouldBeginWith as bs = take (length bs) as `shouldBe` bs

genSpec :: forall t .
  ( Eq                t
  , BitPrint          t
  , FromForeignRegion t
  , TestBit           t
  , BitLength         t
  , Rank0             t
  , Rank1             t
  , IsString          (JsonCursor BS.ByteString t)
  , HasJsonCursorType (JsonCursor BS.ByteString t)
  , Show              t)
  => String -> t -> SpecWith ()
genSpec t _ = do
  describe ("Cursor for (" ++ t ++ ")") $ do
    it "initialises to beginning of empty object" $ do
      let cursor = "{}" :: JsonCursor BS.ByteString t
      jsonCursorType cursor `shouldBe` JsonCursorObject
    it "initialises to beginning of empty object preceded by spaces" $ do
      let cursor = " {}" :: JsonCursor BS.ByteString t
      jsonCursorType cursor `shouldBe` JsonCursorObject
    it "initialises to beginning of number" $ do
      let cursor = "1234" :: JsonCursor BS.ByteString t
      jsonCursorType cursor `shouldBe` JsonCursorNumber
    it "initialises to beginning of string" $ do
      let cursor = "\"Hello\"" :: JsonCursor BS.ByteString t
      jsonCursorType cursor `shouldBe` JsonCursorString
    it "initialises to beginning of array" $ do
      let cursor = "[]" :: JsonCursor BS.ByteString t
      jsonCursorType cursor `shouldBe` JsonCursorArray
    it "initialises to beginning of boolean true" $ do
      let cursor = "true" :: JsonCursor BS.ByteString t
      jsonCursorType cursor `shouldBe` JsonCursorBool
    it "initialises to beginning of boolean false" $ do
      let cursor = "false" :: JsonCursor BS.ByteString t
      jsonCursorType cursor `shouldBe` JsonCursorBool
    it "initialises to beginning of null" $ do
      let cursor = "null" :: JsonCursor BS.ByteString t
      jsonCursorType cursor `shouldBe` JsonCursorNull
    it "cursor can navigate to first child of array" $ do
      let cursor = "[null]" :: JsonCursor BS.ByteString t
      jsonCursorType (fc cursor) `shouldBe` JsonCursorNull
    it "cursor can navigate to second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t
      jsonCursorType ((ns . fc) cursor) `shouldBe` JsonCursorObject
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t
      jsonCursorType ((fc . ns . fc) cursor) `shouldBe` JsonCursorString
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t
      jsonCursorType ((ns . fc . ns . fc) cursor)  `shouldBe` JsonCursorNumber
    it "depth at top" $ do
      let cursor = "[null]" :: JsonCursor BS.ByteString t
      cd cursor `shouldBe` 1
    it "depth at first child of array" $ do
      let cursor = "[null]" :: JsonCursor BS.ByteString t
      cd (fc cursor) `shouldBe` 2
    it "depth at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t
      cd ((ns . fc) cursor) `shouldBe` 2
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t
      cd ((fc . ns . fc) cursor) `shouldBe` 3
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t
      cd ((ns . fc . ns . fc) cursor)  `shouldBe` 3
    it "can navigate down and forwards" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/Resources/sample.json" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString t
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
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString t
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
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString t
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
