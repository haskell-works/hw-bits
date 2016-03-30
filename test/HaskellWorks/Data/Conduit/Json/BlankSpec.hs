{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Conduit.Json.BlankSpec (spec) where

import qualified Data.ByteString                      as BS
import           HaskellWorks.Data.Conduit.Json.Blank
import           HaskellWorks.Data.Conduit.List
import           Test.Hspec

{-# ANN module ("HLint: ignore Redundant do" :: String) #-}

whenBlankedEscapedShouldBe :: BS.ByteString -> BS.ByteString -> Spec
whenBlankedEscapedShouldBe original expected = do
  it (show original ++ " when blanked escaped should be " ++ show expected) $ do
    BS.concat (runListConduit [original] blankEscapedChars) `shouldBe` expected

whenBlankedStringsShouldBe :: BS.ByteString -> BS.ByteString -> Spec
whenBlankedStringsShouldBe original expected = do
  it (show original ++ " when blanked strings should be " ++ show expected) $ do
    BS.concat (runListConduit [original] blankStrings) `shouldBe` expected

whenBlankedNumbersShouldBe :: BS.ByteString -> BS.ByteString -> Spec
whenBlankedNumbersShouldBe original expected = do
  it (show original ++ " when blanked numbers should be " ++ show expected) $ do
    BS.concat (runListConduit [original] blankNumbers) `shouldBe` expected

whenBlankedIdentifiersShouldBe :: BS.ByteString -> BS.ByteString -> Spec
whenBlankedIdentifiersShouldBe original expected = do
  it (show original ++ " when blanked identifiers should be " ++ show expected) $ do
    BS.concat (runListConduit [original] blankIdentifiers) `shouldBe` expected

whenBlankedJsonShouldBe :: BS.ByteString -> BS.ByteString -> Spec
whenBlankedJsonShouldBe original expected = do
  it (show original ++ " when blanked json should be " ++ show expected) $ do
    BS.concat (runListConduit [original] blankJson) `shouldBe` expected

spec :: Spec
spec = describe "HaskellWorks.Data.Conduit.Json.BlankSpec" $ do
  describe "Can blank escapes" $ do
    ""          `whenBlankedEscapedShouldBe` ""
    "\\\\"      `whenBlankedEscapedShouldBe` "\\_"
    "\\\\\\"    `whenBlankedEscapedShouldBe` "\\_\\"
    " \\\\\\"   `whenBlankedEscapedShouldBe` " \\_\\"
    " \\n\\\\"  `whenBlankedEscapedShouldBe` " \\_\\_"
  describe "Can blank strings" $ do
    ""              `whenBlankedStringsShouldBe` ""
    "\"\""          `whenBlankedStringsShouldBe` "()"
    "\" \""         `whenBlankedStringsShouldBe` "( )"
    "\" a \""       `whenBlankedStringsShouldBe` "(   )"
    " \"a \" x"     `whenBlankedStringsShouldBe` " (  ) x"
    " \"a\"b\"c\"d" `whenBlankedStringsShouldBe` " ( )b( )d"
  describe "Can blank numbers" $ do
    ""              `whenBlankedNumbersShouldBe` ""
    "1"             `whenBlankedNumbersShouldBe` "1"
    "11"            `whenBlankedNumbersShouldBe` "10"
    "00"            `whenBlankedNumbersShouldBe` "10"
    "00"            `whenBlankedNumbersShouldBe` "10"
    "-0.12e+34"     `whenBlankedNumbersShouldBe` "100000000"
    "10.12E-34 "    `whenBlankedNumbersShouldBe` "100000000 "
    "10.12E-34 12"  `whenBlankedNumbersShouldBe` "100000000 10"
    " 10.12E-34 -1" `whenBlankedNumbersShouldBe` " 100000000 10"
  describe "Can blank identifiers" $ do
    ""              `whenBlankedIdentifiersShouldBe` ""
    "a"             `whenBlankedIdentifiersShouldBe` "a"
    "z"             `whenBlankedIdentifiersShouldBe` "z"
    " Aaa "         `whenBlankedIdentifiersShouldBe` " A__ "
    " Za def "      `whenBlankedIdentifiersShouldBe` " Z_ d__ "
  describe "Can blank json" $ do
    ""                                    `whenBlankedJsonShouldBe` ""
    " { \"ff\": 1.0, [\"\", true], null}" `whenBlankedJsonShouldBe` " { (  ): 100, [(), true], null}"
