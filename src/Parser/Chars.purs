module Parser.Chars
  ( alpha
  , anyChar
  , char
  , digit
  , int
  , nat
  , neg
  , satisfy
  , spaces
  , string
  ) where

import Prelude

import Control.Alt ((<|>))
import Data.CodePoint.Unicode (isAlpha, isDecDigit, isSpace)
import Data.Either (Either(..))
import Data.Foldable (foldMap)
import Data.Int (fromString)
import Data.List (many, some)
import Data.Maybe (Maybe(..))
import Data.String (CodePoint, codePointFromChar)
import Data.String.CodeUnits (fromCharArray, singleton, toCharArray)
import Data.Traversable (sequence)
import Data.Tuple (Tuple(..))
import Parser (ParseError(..), Parser(..), consume, failWith)

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

satisfyCodePoint :: (CodePoint -> Boolean) -> Parser Char
satisfyCodePoint pred = satisfy $ codePointFromChar >>> pred

digit :: Parser Char
digit = do
  satisfyCodePoint isDecDigit

char :: Char -> Parser Char
char c = satisfy (_ == c)

string :: String -> Parser String
string s = s # toCharArray <#> char # sequence <#> fromCharArray

spaces :: Parser Unit
spaces = do
  _ <- many $ satisfyCodePoint isSpace
  pure unit

alpha :: Parser Char
alpha = satisfyCodePoint isAlpha

nat :: Parser Int
nat = do
  digits <- some digit
  case digits # (foldMap singleton) # fromString of
    Just n -> pure n
    Nothing -> failWith UnexpectedToken

neg :: Parser Int
neg = do
  _ <- char '-'
  negate <$> nat

int :: Parser Int
int = do
  nat <|> neg
