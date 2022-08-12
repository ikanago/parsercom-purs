module Test.Chars where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Parser (ParseError(..))
import Parser.Chars (anyChar, char, digit, satisfy)
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
      assertParser (satisfy (_ == '1')) "23" (Left UnexpectedToken) 1

  describe "digit" do
    it "basic" do
      assertParser digit "123" (Right '1') 1
    it "fail" do
      assertParser digit "abc" (Left UnexpectedToken) 1

  describe "char" do
    it "basic" do
      assertParser (char 'a') "abc" (Right 'a') 1
    it "fail" do
      assertParser (char 'x') "abc" (Left UnexpectedToken) 1
