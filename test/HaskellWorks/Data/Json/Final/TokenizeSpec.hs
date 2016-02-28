{-# LANGUAGE OverloadedStrings #-}

module HaskellWorks.Data.Json.Final.TokenizeSpec (spec) where

import           HaskellWorks.Data.Json.Final.Tokenize
import           HaskellWorks.Data.Json.Token
import           Test.Hspec
import qualified Data.Attoparsec.ByteString.Char8      as BC
import qualified Data.Attoparsec.Types                 as T
import           Data.ByteString                       as BS

data Complete i r
  = Fail i [String] String
  | Done i r
  deriving (Eq, Show)

data Incomplete i r
  = Partial (i -> Result i r)

data Result i r = Either (Incomplete i r) (Complete i r)

completeOf :: T.IResult i r -> Complete i r
completeOf tr = case tr of
  T.Done i r                -> Done i r
  T.Fail i contexts message -> Fail i contexts message
  T.Partial f               -> error "Moo"

moo :: ByteString -> Either String JsonToken
moo text = BC.parseOnly parseJsonToken text

spec :: Spec
spec = describe "Data.Conduit.Succinct.JsonSpec" $ do
  describe "When parsing single token at beginning of text" $ do
    it "Empty Json should produce no bits" $
      moo "" `shouldBe` Left "not enough input"
    it "Json with one space should produce whitespace token" $
      moo " " `shouldBe` Right JsonTokenWhitespace
    it "Json with two spaces should produce whitespace token" $
      moo "  " `shouldBe` Right JsonTokenWhitespace
    it "Spaces and newlines should produce no bits" $
      moo "  \n \r \t " `shouldBe` Right JsonTokenWhitespace
    it "`null` at beginning should produce one bit" $
      moo "null " `shouldBe` Right JsonTokenNull
    it "number at beginning should produce one bit" $
      moo "1234 " `shouldBe` Right (JsonTokenNumber 1234)
    it "false at beginning should produce one bit" $
      moo "false " `shouldBe` Right (JsonTokenBoolean False)
    it "true at beginning should produce one bit" $
      moo "true " `shouldBe` Right (JsonTokenBoolean True)
    it "string at beginning should produce one bit" $
      moo "\"hello\" " `shouldBe` Right (JsonTokenString "hello")
    it "left brace at beginning should produce one bit" $
      moo "{ " `shouldBe` Right JsonTokenBraceL
    it "right brace at beginning should produce one bit" $
      moo "} " `shouldBe` Right JsonTokenBraceR
    it "left bracket at beginning should produce one bit" $
      moo "[ " `shouldBe` Right JsonTokenBracketL
    it "right bracket at beginning should produce one bit" $
      moo "] " `shouldBe` Right JsonTokenBracketR
    it "right bracket at beginning should produce one bit" $
      moo ": " `shouldBe` Right JsonTokenColon
    it "right bracket at beginning should produce one bit" $
      moo ", " `shouldBe` Right JsonTokenComma
