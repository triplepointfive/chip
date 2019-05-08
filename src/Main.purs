module Main where

import Component.Game as Game

import Effect (Effect)
import Prelude

import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Game.component unit body
