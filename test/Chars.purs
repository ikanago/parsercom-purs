module Test.Chars where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Parser.Chars (anyChar)
import Test.Util (assertParser)
import Test.Spec (SpecT, describe, it)

chars :: forall m1 g2. Monad m1 => MonadThrow Error g2 => SpecT g2 Unit m1 Unit
chars = do
  describe "anyChar" do
    it "basic" do
      assertParser anyChar "str" (Right 's') 1
    it "parsing empty string causes an error" do
      assertParser anyChar "" (Left "End of input") 0
