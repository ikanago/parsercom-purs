module Parser
  ( ParseError
  , ParseState(..)
  , Parser(..)
  , TokenPos
  , consume
  , evalParserStr
  , peek
  , runParser
  , runParserStr
  ) where

import Prelude

import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (charAt)
import Data.Tuple (Tuple(..), fst)

type TokenPos = Int

initPos :: TokenPos
initPos = 0

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

type ParseError = String

newtype Parser a = Parser ((ParseState) -> (Tuple (Either ParseError a) ParseState))

runParser :: forall a. Parser a -> ParseState -> (Tuple (Either ParseError a) ParseState)
runParser (Parser p) s = p s

runParserStr :: forall a. Parser a -> String -> (Tuple (Either ParseError a) ParseState)
runParserStr p s = runParser p (ParseState s initPos)

evalParserStr :: forall a. Parser a -> String -> Either ParseError a
evalParserStr p s = fst $ runParserStr p s

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
