{-# OPTIONS_GHC -fno-warn-incomplete-patterns #-}
{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Succinct.CursorSpec(spec) where

import Control.Monad
import Control.Monad.IO.Class
import           HaskellWorks.Data.Conduit.Json
import           HaskellWorks.Data.Json.Succinct
import           HaskellWorks.Data.Json.Succinct.Cursor
import           Test.Hspec

spec :: Spec
spec = describe "HaskellWorks.Data.Json.Succinct.CursorSpec" $ do
  describe "When running markerToByteString" $ do
    let json = "{}" :: JsonCursor String [Bool]
    it "Marker at zero gives \"\1\"" $ do
      liftIO $ print json
      jsonCursorType json `shouldBe` JsonCursorObject
