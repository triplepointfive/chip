module Main where

import Component.Game (gameComponent)

import Effect (Effect)
import Prelude

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI gameComponent unit body
