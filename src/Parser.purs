module Parser
  ( ParseError
  , ParseState(..)
  , Parser(..)
  , TokenPos
  , consume
  , parseStr
  , peek
  , runParser
  ) where

import Prelude

import Data.Array (index)
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..), fst)

type TokenPos = Int

initPos :: TokenPos
initPos = 0

data ParseState t = ParseState (Array t) TokenPos

-- instance showParseState :: Show ParseState where

peek :: forall t. ParseState t -> Maybe t
peek (ParseState array pos) = index array pos

consume :: forall t. ParseState t -> Tuple (Maybe t) (ParseState t)
consume s@(ParseState array pos) = Tuple a (ParseState array pos')
  where
  a = peek s
  pos' = case a of
    Just _ -> pos + 1
    Nothing -> pos

type ParseError = String

newtype Parser t a = Parser ((ParseState t) -> (Tuple (Either ParseError a) (ParseState t)))

runParser :: forall t a. Parser t a -> ParseState t -> (Tuple (Either ParseError a) (ParseState t))
runParser (Parser p) s = p s

parseStr :: forall a. Parser Char a -> String -> Either ParseError a
parseStr p s = fst $ runParser p (ParseState (toCharArray s) initPos)

instance functorParser :: Functor (Parser t) where
  map f p = Parser $ \s ->
    let
      (Tuple res s') = runParser p s
    in
      Tuple (f <$> res) s'

instance applyParser :: Apply (Parser t) where
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

instance applicativeParser :: Applicative (Parser t) where
  pure x = Parser $ \s -> Tuple (Right x) s

instance bindParser :: Bind (Parser t) where
  bind p f = Parser $ \s ->
    let
      (Tuple res s') = runParser p s
    in
      case res of
        Left e -> Tuple (Left e) s'
        Right res -> runParser (f res) s'

instance monadParser :: Monad (Parser t)
