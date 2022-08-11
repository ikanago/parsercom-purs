module Parser.Chars
  ( anyChar
  , satisfy
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Parser (Parser(..), consume, failWith)

-- | Parse the first character in the state.
anyChar :: Parser Char
anyChar = Parser $ \s ->
  let
    (Tuple c s') = consume s
  in
    case c of
      Just c -> Tuple (Right c) s'
      Nothing -> Tuple (Left "End of input") s'

-- | Parse if a predicate is true for the first character in the state.
-- | This combinator consumes the first character even if the predicate is not satisfied.
satisfy :: (Char -> Boolean) -> Parser Char
satisfy pred = do
  c <- anyChar
  if pred c then pure c else failWith "Given predicate is not satisfied"
