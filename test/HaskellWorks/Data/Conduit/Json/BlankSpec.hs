{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Conduit.Json.BlankSpec (spec) where

import qualified Data.ByteString                      as BS
import           HaskellWorks.Data.Conduit.Json.Blank
import           HaskellWorks.Data.Conduit.List
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

whenBlankedStringsShouldBe :: BS.ByteString -> BS.ByteString -> Spec
whenBlankedStringsShouldBe original expected = do
  it (show original ++ " when blanked escaped should be " ++ show expected) $ do
    BS.concat (runListConduit blankStrings [original]) `shouldBe` expected

whenBlankedIdentifiersShouldBe :: BS.ByteString -> BS.ByteString -> Spec
whenBlankedIdentifiersShouldBe original expected = do
  it (show original ++ " when blanked identifiers should be " ++ show expected) $ do
    BS.concat (runListConduit blankIdentifiers [original]) `shouldBe` expected

whenBlankedJsonShouldBe :: BS.ByteString -> BS.ByteString -> Spec
whenBlankedJsonShouldBe original expected = do
  it (show original ++ " when blanked json should be " ++ show expected) $ do
    BS.concat (runListConduit blankJson [original]) `shouldBe` expected

spec :: Spec
spec = describe "HaskellWorks.Data.Conduit.Json.BlankSpec" $ do
  describe "Can blank strings" $ do
    "\"\""          `whenBlankedStringsShouldBe` "()"
    "\"\\\\\""      `whenBlankedStringsShouldBe` "(  )"
    "\"\\\\\\\""    `whenBlankedStringsShouldBe` "(    "
    "\" \\\\\\\""   `whenBlankedStringsShouldBe` "(     "
    "\" \\n\\\\\""  `whenBlankedStringsShouldBe` "(     )"
    ""              `whenBlankedStringsShouldBe` ""
    "\"\""          `whenBlankedStringsShouldBe` "()"
    "\" \""         `whenBlankedStringsShouldBe` "( )"
    "\" a \""       `whenBlankedStringsShouldBe` "(   )"
    " \"a \" x"     `whenBlankedStringsShouldBe` " (  ) x"
    " \"a\"b\"c\"d" `whenBlankedStringsShouldBe` " ( )b( )d"
  describe "Can blank numbers" $ do
    ""              `whenBlankedStringsShouldBe` ""
    "1"             `whenBlankedStringsShouldBe` "1"
    "11"            `whenBlankedStringsShouldBe` "10"
    "00"            `whenBlankedStringsShouldBe` "10"
    "00"            `whenBlankedStringsShouldBe` "10"
    "-0.12e+34"     `whenBlankedStringsShouldBe` "100000000"
    "10.12E-34 "    `whenBlankedStringsShouldBe` "100000000 "
    "10.12E-34 12"  `whenBlankedStringsShouldBe` "100000000 10"
    " 10.12E-34 -1" `whenBlankedStringsShouldBe` " 100000000 10"
  describe "Can blank identifiers" $ do
    ""              `whenBlankedIdentifiersShouldBe` ""
    "a"             `whenBlankedIdentifiersShouldBe` "a"
    "z"             `whenBlankedIdentifiersShouldBe` "z"
    " Aaa "         `whenBlankedIdentifiersShouldBe` " A__ "
    " Za def "      `whenBlankedIdentifiersShouldBe` " Z_ d__ "
  describe "Can blank json" $ do
    ""                                    `whenBlankedJsonShouldBe` ""
    " { \"ff\": 1.0, [\"\", true], null}" `whenBlankedJsonShouldBe` " { (  ): 100, [(), t___], n___}"
