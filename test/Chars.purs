module Test.Chars
  ( chars
  ) where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Parser (evalParserStr)
import Parser.Chars (anyChar)
import Test.Spec (SpecT, describe, it)
import Test.Spec.Assertions (shouldEqual)

chars :: forall m1 g2. Monad m1 => MonadThrow Error g2 => SpecT g2 Unit m1 Unit
chars = do
  describe "anyChar" do
    it "basic" do
      evalParserStr anyChar "str" `shouldEqual` Right 's'
