module Test.Combinator where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Parser.Chars (char, string)
import Parser.Combinator (interpose)
import Test.Spec (SpecT, describe, it)
import Test.Util (assertParser)

combinator :: forall m1 g2. Monad m1 => MonadThrow Error g2 => SpecT g2 Unit m1 Unit
combinator = do
  describe "interpose" do
    it "basic" do
      assertParser (interpose (char '(') (char ')') (string "str")) "(str)" (Right "str") 5
