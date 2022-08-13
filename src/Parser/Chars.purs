module Parser.Chars
  ( anyChar
  , char
  , digit
  , satisfy
  , string
  )
  where

import Prelude

import Data.Either (Either(..))
import Data.List (fromFoldable, toUnfoldable)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (fromCharArray, toCharArray)
import Data.Tuple (Tuple(..))
import Parser (Parser(..), ParseError(..), consume, failWith)
import Parser.Combinator (sequence)

-- | Parse the first character in the state.
anyChar :: Parser Char
anyChar = Parser $ \s ->
  let
    (Tuple c s') = consume s
  in
    case c of
      Just c -> Tuple (Right c) s'
      Nothing -> Tuple (Left EndOfInput) s'

-- | Parse if a predicate is true for the first character in the state.
-- | This combinator consumes the first character even if the predicate is not satisfied.
satisfy :: (Char -> Boolean) -> Parser Char
satisfy pred = do
  c <- anyChar
  if pred c then pure c else failWith UnexpectedToken

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
string s = s # toCharArray # fromFoldable <#> char # sequence <#> (toUnfoldable >>> fromCharArray)
