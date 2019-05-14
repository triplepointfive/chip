module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Component.Game as Game
import Lib (getJSON)

-- | Outputs main game component
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  getJSON "levels/1.json" (\level -> runUI (Game.component level 1) unit body)
