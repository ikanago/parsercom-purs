module Test.Parser where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either(..))
import Effect.Exception (Error)
import Parser.Chars (anyChar)
import Test.Spec (SpecT, describe, it)
import Test.Util (assertParser)

parser :: forall m1 g2. Monad m1 => MonadThrow Error g2 => SpecT g2 Unit m1 Unit
parser = do
  describe "monad" do
    it "two parsers in a row" do
      assertParser any2Chars "str" (Right [ 's', 't' ]) 2
  where
  any2Chars = do
    c1 <- anyChar
    c2 <- anyChar
    pure [ c1, c2 ]
