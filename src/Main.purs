module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Euler

main :: Effect Unit
main = do
  log $ "Hello sailor!" <> show answer
