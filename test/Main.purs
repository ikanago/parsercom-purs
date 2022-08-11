module Test.Main where

import Prelude

import Effect (Effect)
import Effect.Class.Console (logShow)
import Parser (parseStr)
import Parser.Chars (anyChar)

main :: Effect Unit
main = do
  let p = anyChar >>= (\c -> anyChar >>= \d -> pure [ c, d ])
  logShow $ parseStr p "str"
