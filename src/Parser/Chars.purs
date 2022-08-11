module Parser.Chars where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Parser (Parser(..), consume)

anyChar :: Parser Char Char
anyChar = Parser $ \s ->
  let
    (Tuple c s') = consume s
  in
    case c of
      Just c -> Tuple (Right c) s'
      Nothing -> Tuple (Left "End of input") s'
