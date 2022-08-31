module Parser.Combinator
  ( interpose
  , sepBy
  , sepBy1
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.List (List(..), (:), many)
import Parser (Parser)

-- Parses opening characters, inner contents and closing characters.
interpose :: forall a b c. Parser a -> Parser b -> Parser c -> Parser c
interpose popen pclose p = popen *> p <* pclose

-- Parses one or more occurances of `p`, separated by `sep`.
sepBy :: forall a b. Parser a -> Parser b -> Parser (List a)
sepBy p sep = sepBy1 p sep <|> pure Nil

-- Parses one or more occurances of `p`, separated by `sep`.
sepBy1 :: forall a b. Parser a -> Parser b -> Parser (List a)
sepBy1 p sep = do
  x <- p
  xs <- many (sep *> p)
  pure (x : xs)
