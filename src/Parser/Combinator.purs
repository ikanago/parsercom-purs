module Parser.Combinator
  ( interpose
  ) where

import Prelude

import Parser (Parser)

-- Parse opening characters, inner contents and closing characters.
interpose :: forall a b c. Parser a -> Parser b -> Parser c -> Parser c
interpose popen pclose p = popen *> p <* pclose
