{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module HaskellWorks.Data.Json.Succinct.CursorSpec(spec) where

import qualified Data.ByteString                                            as BS
import           Data.String
import qualified Data.Vector.Storable                                       as DVS
import           Data.Word
import           HaskellWorks.Data.Bits.BitShow
import           HaskellWorks.Data.Bits.BitShown
import           HaskellWorks.Data.FromForeignRegion
import           HaskellWorks.Data.Json.Succinct.Cursor                     as C
import           HaskellWorks.Data.Json.Succinct.Cursor.Internal
import           HaskellWorks.Data.Json.Succinct.Cursor.JsonCursorType
import           HaskellWorks.Data.Json.Token
import qualified HaskellWorks.Data.Succinct.BalancedParens.Internal         as BP
import           HaskellWorks.Data.Succinct.BalancedParens.Simple
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank0
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Rank1
import           HaskellWorks.Data.Succinct.RankSelect.Binary.Basic.Select1
import           System.IO.MMap
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: redundant bracket"          :: String) #-}

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.CursorSpec" $ do
  let fc = C.firstChild
  let ns = C.nextSibling
  let cd = C.depth
  describe "Cursor for [Bool]" $ do
    it "initialises to beginning of empty object" $ do
      let cursor = "{}" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` JsonCursorObject
    it "initialises to beginning of empty object preceded by spaces" $ do
      let cursor = " {}" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` JsonCursorObject
    it "initialises to beginning of number" $ do
      let cursor = "1234" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` JsonCursorNumber
    it "initialises to beginning of string" $ do
      let cursor = "\"Hello\"" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` JsonCursorString
    it "initialises to beginning of array" $ do
      let cursor = "[]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` JsonCursorArray
    it "initialises to beginning of boolean true" $ do
      let cursor = "true" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` JsonCursorBool
    it "initialises to beginning of boolean false" $ do
      let cursor = "false" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` JsonCursorBool
    it "initialises to beginning of null" $ do
      let cursor = "null" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType cursor `shouldBe` JsonCursorNull
    it "cursor can navigate to first child of array" $ do
      let cursor = "[null]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType (fc cursor) `shouldBe` JsonCursorNull
    it "cursor can navigate to second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType ((ns . fc) cursor) `shouldBe` JsonCursorObject
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType ((fc . ns . fc) cursor) `shouldBe` JsonCursorString
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      jsonCursorType ((ns . fc . ns . fc) cursor)  `shouldBe` JsonCursorNumber
    it "depth at top" $ do
      let cursor = "[null]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd cursor `shouldBe` 1
    it "depth at first child of array" $ do
      let cursor = "[null]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd (fc cursor) `shouldBe` 2
    it "depth at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd ((ns . fc) cursor) `shouldBe` 2
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd ((fc . ns . fc) cursor) `shouldBe` 3
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor String (BitShown [Bool]) (SimpleBalancedParens [Bool])
      cd ((ns . fc . ns . fc) cursor)  `shouldBe` 3
  genSpec "DVS.Vector Word8"  (undefined :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8)))
  genSpec "DVS.Vector Word16" (undefined :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16)))
  genSpec "DVS.Vector Word32" (undefined :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32)))
  genSpec "DVS.Vector Word64" (undefined :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64)))
  it "Loads same Json consistentally from different backing vectors" $ do
    let cursor8   = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word8)) (SimpleBalancedParens (DVS.Vector Word8))
    let cursor16  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word16)) (SimpleBalancedParens (DVS.Vector Word16))
    let cursor32  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word32)) (SimpleBalancedParens (DVS.Vector Word32))
    let cursor64  = "{\n    \"widget\": {\n        \"debug\": \"on\"  } }" :: JsonCursor BS.ByteString (BitShown (DVS.Vector Word64)) (SimpleBalancedParens (DVS.Vector Word64))
    cursorText cursor8 `shouldBe` cursorText cursor16
    cursorText cursor8 `shouldBe` cursorText cursor32
    cursorText cursor8 `shouldBe` cursorText cursor64
    let ic8   = bitShow $ interests cursor8
    let ic16  = bitShow $ interests cursor16
    let ic32  = bitShow $ interests cursor32
    let ic64  = bitShow $ interests cursor64
    ic16 `shouldBeginWith` ic8
    ic32 `shouldBeginWith` ic16
    ic64 `shouldBeginWith` ic32

shouldBeginWith :: (Eq a, Show a) => [a] -> [a] -> IO ()
shouldBeginWith as bs = take (length bs) as `shouldBe` bs

genSpec :: forall t u.
  ( Eq                t
  , BitShow          t
  , Select1           t
  , Show              t
  , Eq                u
  , BitShow          u
  , Rank0             u
  , Rank1             u
  , Show              u
  , BP.BalancedParens u
  , FromForeignRegion (JsonCursor BS.ByteString t u)
  , IsString          (JsonCursor BS.ByteString t u)
  , HasJsonCursorType (JsonCursor BS.ByteString t u))
  => String -> (JsonCursor BS.ByteString t u) -> SpecWith ()
genSpec t _ = do
  let fc = C.firstChild
  let ns = C.nextSibling
  let pn = C.parent
  let cd = C.depth
  let ss = C.subtreeSize
  describe ("Cursor for (" ++ t ++ ")") $ do
    it "initialises to beginning of empty object" $ do
      let cursor = "{}" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` JsonCursorObject
    it "initialises to beginning of empty object preceded by spaces" $ do
      let cursor = " {}" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` JsonCursorObject
    it "initialises to beginning of number" $ do
      let cursor = "1234" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` JsonCursorNumber
    it "initialises to beginning of string" $ do
      let cursor = "\"Hello\"" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` JsonCursorString
    it "initialises to beginning of array" $ do
      let cursor = "[]" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` JsonCursorArray
    it "initialises to beginning of boolean true" $ do
      let cursor = "true" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` JsonCursorBool
    it "initialises to beginning of boolean false" $ do
      let cursor = "false" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` JsonCursorBool
    it "initialises to beginning of null" $ do
      let cursor = "null" :: JsonCursor BS.ByteString t u
      jsonCursorType cursor `shouldBe` JsonCursorNull
    it "cursor can navigate to first child of array" $ do
      let cursor = "[null]" :: JsonCursor BS.ByteString t u
      jsonCursorType (firstChild cursor) `shouldBe` JsonCursorNull
    it "cursor can navigate to second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t u
      jsonCursorType ((ns . fc) cursor) `shouldBe` JsonCursorObject
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t u
      jsonCursorType ((fc . ns . fc) cursor) `shouldBe` JsonCursorString
    it "cursor can navigate to first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t u
      jsonCursorType ((ns . fc . ns . fc) cursor)  `shouldBe` JsonCursorNumber
    it "depth at top" $ do
      let cursor = "[null]" :: JsonCursor BS.ByteString t u
      cd cursor `shouldBe` 1
    it "depth at first child of array" $ do
      let cursor = "[null]" :: JsonCursor BS.ByteString t u
      cd (fc cursor) `shouldBe` 2
    it "depth at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t u
      cd ((ns . fc) cursor) `shouldBe` 2
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t u
      cd ((fc . ns . fc) cursor) `shouldBe` 3
    it "depth at first child of object at second child of array" $ do
      let cursor = "[null, {\"field\": 1}]" :: JsonCursor BS.ByteString t u
      cd ((ns . fc . ns . fc) cursor)  `shouldBe` 3
    it "can navigate down and forwards" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/Resources/sample.json" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString t u
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
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString t u
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
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString t u
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
    it "can get token at cursor" $ do
      (fptr, offset, size) <- mmapFileForeignPtr "test/Resources/sample.json" ReadOnly Nothing
      let cursor = fromForeignRegion (fptr, offset, size) :: JsonCursor BS.ByteString t u
      jsonTokenAt                                                              cursor  `shouldBe` JsonTokenBraceL
      jsonTokenAt ((                                                       fc) cursor) `shouldBe` JsonTokenString "widget"
      jsonTokenAt ((                                                  ns . fc) cursor) `shouldBe` JsonTokenBraceL
      jsonTokenAt ((                                             fc . ns . fc) cursor) `shouldBe` JsonTokenString "debug"
      jsonTokenAt ((                                        ns . fc . ns . fc) cursor) `shouldBe` JsonTokenString "on"
      jsonTokenAt ((                                   ns . ns . fc . ns . fc) cursor) `shouldBe` JsonTokenString "window"
      jsonTokenAt ((                              ns . ns . ns . fc . ns . fc) cursor) `shouldBe` JsonTokenBraceL
      jsonTokenAt ((                         fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` JsonTokenString "title"
      jsonTokenAt ((                    ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` JsonTokenString "Sample Konfabulator Widget"
      jsonTokenAt ((               ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` JsonTokenString "name"
      jsonTokenAt ((          ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` JsonTokenString "main_window"
      jsonTokenAt ((     ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` JsonTokenString "width"
      jsonTokenAt ((ns . ns . ns . ns . ns . fc . ns . ns . ns . fc . ns . fc) cursor) `shouldBe` JsonTokenNumber 500.0
