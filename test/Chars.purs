module Test.Chars where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Parser.Chars (anyChar, satisfy)
import Test.Spec (SpecT, describe, it)
import Test.Util (assertParser)

chars :: forall m1 g2. Monad m1 => MonadThrow Error g2 => SpecT g2 Unit m1 Unit
chars = do
  describe "anyChar" do
    it "basic" do
      assertParser anyChar "str" (Right 's') 1
    it "parsing empty string causes an error" do
      assertParser anyChar "" (Left "End of input") 0

  describe "satisfy" do
    it "basic" do
      assertParser (satisfy \c -> c == '1') "123" (Right '1') 1
    it "predicate is not satisfied" do
      assertParser (satisfy \c -> c == '1') "23" (Left "Given predicate is not satisfied") 1
