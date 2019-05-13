module Main
  ( main
  ) where

import Prelude

import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Component.Game as Game

-- | Outputs main game component
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI Game.component unit body
