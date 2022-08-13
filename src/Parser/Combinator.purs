module Parser.Combinator where

import Prelude

import Control.Apply (lift2)
import Data.List (List(..))
import Parser (Parser)

sequence :: forall a. List (Parser a) -> Parser (List a)
sequence ps = case ps of
  Nil -> pure Nil
  Cons head tail -> (lift2 Cons) head (sequence tail)
