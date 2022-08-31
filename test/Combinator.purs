module Test.Combinator where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.List (List(..), (:), many)
import Effect.Exception (Error)
import Parser (ParseError(..))
import Parser.Chars (char, string)
import Parser.Combinator (interpose, sepBy, sepBy1)
import Test.Spec (SpecT, describe, it)
import Test.Util (assertParser)

combinator :: forall m1 g2. Monad m1 => MonadThrow Error g2 => SpecT g2 Unit m1 Unit
combinator = do
  describe "interpose" do
    it "basic" do
      assertParser (interpose (char '(') (char ')') (string "str")) "(str)" (Right "str") 5

  describe "many" do
    it "basic" do
      assertParser (many (char 'a')) "aaab" (Right $ 'a' : 'a' : 'a' : Nil) 3
    it "no match" do
      assertParser (many (char 'b')) "aaab" (Right Nil) 0
    it "one match" do
      assertParser (many (char 'a')) "ab" (Right $ 'a' : Nil) 1

  describe "sepBy" do
    it "basic" do
      assertParser (sepBy (string "abc") (char ',')) "abc,abc,abc,def" (Right $ "abc" : "abc" : "abc" : Nil) 11
    it "no match" do
      assertParser (sepBy (string "abc") (char ',')) "def" (Right Nil) 0
    it "one match" do
      assertParser (sepBy (string "abc") (char ',')) "abc,def" (Right $ "abc" : Nil) 3

  describe "sepBy1" do
    it "basic" do
      assertParser (sepBy1 (string "abc") (char ',')) "abc,abc,abc,def" (Right $ "abc" : "abc" : "abc" : Nil) 11
    it "no match" do
      assertParser (sepBy1 (string "abc") (char ',')) "def" (Left UnexpectedToken) 0
    it "one match" do
      assertParser (sepBy1 (string "abc") (char ',')) "abc,def" (Right $ "abc" : Nil) 3
