module Test.Util where

import Prelude

import Control.Monad.Error.Class (class MonadThrow)
import Data.Either (Either)
import Data.Tuple (Tuple(..))
import Effect.Aff (Error)
import Parser (ParseError, ParseState(..), Parser, TokenPos, runParserStr)
import Test.Spec.Assertions (shouldEqual)

assertParser :: forall m a. MonadThrow Error m => Show a => Eq a => Parser a -> String -> (Either ParseError a) -> TokenPos -> m Unit
assertParser p input res pos = shouldEqual (runParserStr p input) (Tuple res st)
  where
  st = ParseState input pos
