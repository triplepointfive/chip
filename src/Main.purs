module Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)

import Component.Game as Game
import Lib (getJSON)

-- | Outputs main game component
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  result <- getJSON "levels/1.json"
  case result of
    Just blank -> do
      _ <- runUI (Game.component blank 1) unit body
      pure unit
    Nothing ->
      pure unit
