module Test.Parser where

import Prelude

import Control.Alt ((<|>))
import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Parser (ParseError(..), Parser)
import Parser.Chars (anyChar, char)
import Test.Spec (SpecT, describe, it)
import Test.Util (assertParser)

any2Chars :: Parser (Array Char)
any2Chars = do
  c1 <- anyChar
  c2 <- anyChar
  pure [ c1, c2 ]

parser :: forall m1 g2. Monad m1 => MonadThrow Error g2 => SpecT g2 Unit m1 Unit
parser = do
  describe "monad" do
    it "two parsers in a row" do
      assertParser any2Chars "str" (Right [ 's', 't' ]) 2

  describe "alt" do
    it "char a or char b" do
      assertParser ((char 'a' <|> char 'b')) "abc" (Right 'a') 1
      assertParser ((char 'b' <|> char 'a')) "abc" (Right 'a') 1
    it "both fail" do
      assertParser ((char 'a' <|> char 'b')) "123" (Left UnexpectedToken) 0
