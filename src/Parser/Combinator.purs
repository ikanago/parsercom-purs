module Parser.Combinator
  ( interpose
  , spaces
  )
  where

import Prelude

import Control.Alt ((<|>))
import Data.List (many)
import Parser (Parser)
import Parser.Chars (char)

-- Parse opening characters, inner contents and closing characters.
interpose :: forall a b c. Parser a -> Parser b -> Parser c -> Parser c
interpose popen pclose p = popen *> p <* pclose

spaces :: Parser Unit
spaces = do
  _ <- many $ (char ' ') <|> char '\n'
  pure unit
