module Parser
  ( ParseError(..)
  , ParseState(..)
  , Parser(..)
  , TokenPos
  , consume
  , evalParserStr
  , failWith
  , peek
  , runParser
  , runParserStr
  ) where

import Prelude

import Control.Alt (class Alt)
import Control.Alternative (class Alternative)
import Control.Lazy (class Lazy)
import Control.Plus (class Plus)
import Data.Either (Either(..))
import Data.List (List)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (charAt)
import Data.Tuple (Tuple(..), fst)

type TokenPos = Int

initPos :: TokenPos
initPos = 0

-- | Represents a current state of parsed input.
-- | TokenPos points to the next character to be evaluated.
data ParseState = ParseState String TokenPos

instance showParseState :: Show ParseState where
  show (ParseState s pos) = (show s) <> " at " <> (show pos)

derive instance eqParseState :: Eq ParseState

peek :: ParseState -> Maybe Char
peek (ParseState str pos) = charAt pos str

consume :: ParseState -> Tuple (Maybe Char) ParseState
consume s@(ParseState array pos) = Tuple a (ParseState array pos')
  where
  a = peek s
  pos' = case a of
    Just _ -> pos + 1
    Nothing -> pos

data ParseError = EndOfInput | UnexpectedToken

instance showParseError :: Show ParseError where
  show err = case err of
    EndOfInput -> "End of input"
    UnexpectedToken -> "Unexpected token"

derive instance eqParseError :: Eq ParseError

-- | The `Parser a` monad with result type `a`.
newtype Parser a = Parser ((ParseState) -> (Tuple (Either ParseError a) ParseState))

-- | Run a parser for a given input.
runParser :: forall a. Parser a -> ParseState -> (Tuple (Either ParseError a) ParseState)
runParser (Parser p) s = p s

-- | Run a parser for a given string input.
runParserStr :: forall a. Parser a -> String -> (Tuple (Either ParseError a) ParseState)
runParserStr p s = runParser p (ParseState s initPos)

-- | Run a parser for a given string input and return only the result.
evalParserStr :: forall a. Parser a -> String -> Either ParseError a
evalParserStr p s = fst $ runParserStr p s

failWith :: forall a. ParseError -> Parser a
failWith err = Parser $ \s ->
  Tuple (Left err) s

instance functorParser :: Functor Parser where
  map f p = Parser $ \s ->
    let
      (Tuple res s') = runParser p s
    in
      Tuple (f <$> res) s'

instance applyParser :: Apply Parser where
  apply fp p = Parser $ \s ->
    let
      (Tuple fp s') = runParser fp s
    in
      case fp of
        Left e -> Tuple (Left e) s'
        Right f ->
          let
            (Tuple res s'') = runParser p s'
          in
            Tuple (f <$> res) s''

instance applicativeParser :: Applicative Parser where
  pure x = Parser $ \s -> Tuple (Right x) s

instance bindParser :: Bind Parser where
  bind p f = Parser $ \s ->
    let
      (Tuple res s') = runParser p s
    in
      case res of
        Left e -> Tuple (Left e) s'
        Right res -> runParser (f res) s'

instance monadParser :: Monad Parser

instance altParser :: Alt Parser where
  alt p1 p2 = Parser $ \s ->
    let
      (Tuple res s') = runParser p1 s
    in
      case res of
        Left _ -> runParser p2 s
        Right res -> Tuple (Right res) s'

instance plusParser :: Plus Parser where
  empty = failWith EndOfInput

instance alternativeParser :: Alternative Parser

instance lazyParser :: Lazy (Parser (List a)) where
  defer g = Parser $ \s -> runParser (g unit) s
