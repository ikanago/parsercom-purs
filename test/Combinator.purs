module Test.Combinator where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Data.List (List(..), (:))
import Effect.Exception (Error)
import Parser.Chars (char, string)
import Parser.Combinator (interpose, many)
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
