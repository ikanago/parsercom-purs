module Test.Chars where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Parser (ParseError(..))
import Parser.Chars (anyChar, char, digit, int, nat, neg, satisfy, spaces, string)
import Test.Spec (SpecT, describe, it)
import Test.Util (assertParser)

chars :: forall m1 g2. Monad m1 => MonadThrow Error g2 => SpecT g2 Unit m1 Unit
chars = do
  describe "anyChar" do
    it "basic" do
      assertParser anyChar "str" (Right 's') 1
    it "parsing empty string causes an error" do
      assertParser anyChar "" (Left EndOfInput) 0

  describe "satisfy" do
    it "basic" do
      assertParser (satisfy (_ == '1')) "123" (Right '1') 1
    it "predicate is not satisfied" do
      assertParser (satisfy (_ == '1')) "23" (Left UnexpectedToken) 0

  describe "digit" do
    it "basic" do
      assertParser digit "123" (Right '1') 1
    it "fail" do
      assertParser digit "abc" (Left UnexpectedToken) 0

  describe "char" do
    it "basic" do
      assertParser (char 'a') "abc" (Right 'a') 1
    it "fail" do
      assertParser (char 'x') "abc" (Left UnexpectedToken) 0

  describe "string" do
    it "basic" do
      assertParser (string "str") "string" (Right "str") 3
    it "fail" do
      assertParser (string "str") "abc" (Left UnexpectedToken) 0

  describe "spaces" do
    it "basic" do
      assertParser spaces "   aa" (Right unit) 3
    it "no match" do
      assertParser spaces "aa" (Right unit) 0
    it "one match" do
      assertParser spaces " aa" (Right unit) 1
    it "new line" do
      assertParser spaces "\naa" (Right unit) 1
    it "space or new line" do
      assertParser spaces " \n \n  aa" (Right unit) 6

  describe "nat" do
    it "basic" do
      assertParser nat "123ab" (Right 123) 3
    it "zero" do
      assertParser nat "0ab" (Right 0) 1

  describe "neg" do
    it "basic" do
      assertParser neg "-123ab" (Right (-123)) 4
    it "zero" do
      assertParser neg "0ab" (Left UnexpectedToken) 0

  describe "int" do
    it "basic" do
      assertParser int "123ab" (Right 123) 3
    it "negative" do
      assertParser int "-123ab" (Right (-123)) 4
    it "zero" do
      assertParser int "0ab" (Right 0) 1
