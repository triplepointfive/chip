module Main
  ( main
  ) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
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
      io <- runUI (Game.component blank 1) unit body
      liftEffect $ void $
          setInterval
              250
              (void $ launchAff $ io.query (H.action Game.Tick))
      pure unit
    Nothing ->
      pure unit
