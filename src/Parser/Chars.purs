module Parser.Chars
  ( anyChar
  , char
  , digit
  , satisfy
  , string
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Parser (ParseError(..), Parser(..), consume)

-- | Parse the first character in the state.
anyChar :: Parser Char
anyChar = satisfy (const true)

-- | Parse if a predicate is true for the first character in the state.
-- | This combinator consumes the first character when succeeded, but leaves the state as it is when failed.
satisfy :: (Char -> Boolean) -> Parser Char
satisfy pred = Parser $ \s ->
  let
    (Tuple c s') = consume s
  in
    case c of
      Nothing -> Tuple (Left EndOfInput) s
      Just c ->
        if pred c then Tuple (Right c) s'
        else Tuple (Left UnexpectedToken) s

digit :: Parser Char
digit = do
  satisfy $ case _ of
    '0' -> true
    '1' -> true
    '2' -> true
    '3' -> true
    '4' -> true
    '5' -> true
    '6' -> true
    '7' -> true
    '8' -> true
    '9' -> true
    _ -> false

char :: Char -> Parser Char
char c = satisfy (_ == c)

string :: String -> Parser String
string s = s # toCharArray <#> char # sequence <#> fromCharArray
