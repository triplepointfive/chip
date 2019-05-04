module Test.Main where

import Prelude

import Effect
import Test.Assert (assert)

main :: Effect Unit
main = do
  assert (123 == 233167)
