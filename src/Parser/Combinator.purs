module Parser.Combinator
  ( interpose
  , many
  ) where

import Prelude

import Data.Either (Either(..))
import Data.List (List(..), (:))
import Data.Tuple (Tuple(..))
import Parser (Parser(..), runParser)

-- Parse opening characters, inner contents and closing characters.
interpose :: forall a b c. Parser a -> Parser b -> Parser c -> Parser c
interpose popen pclose p = popen *> p <* pclose

-- Apply a parser as many time as possible.
many :: forall a. Parser a -> Parser (List a)
many p = Parser $ \s -> many_ s
  where
  many_ s =
    let
      (Tuple res s') = runParser p s
    in
      case res of
        Left _ -> Tuple (Right Nil) s
        Right x ->
          let
            (Tuple res s'') = many_ s'
          in
            case res of
              Left _ -> Tuple (Right (x : Nil)) s'
              Right xs -> Tuple (Right (x : xs)) s''
