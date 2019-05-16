module Main
  ( main
  ) where

import Prelude

import Data.Foldable (traverse_)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff, launchAff)
import Effect.Class (liftEffect)
import Effect.Timer (setInterval)
import Halogen as H
import Halogen.Aff as HA
import Halogen.VDom.Driver (runUI)
import Web.Event.EventTarget as ET
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import Web.HTML.HTMLDocument (HTMLDocument, toEventTarget)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import Component.Game as Game
import Lib (getJSON)

-- | Outputs main game component
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  result <- getJSON "levels/2.json"
  case result of
    Just blank -> do
      game <- runUI (Game.component blank 1) unit body
      liftEffect $ tickInterval game

      document <- H.liftEffect $ DOM.document =<< DOM.window
      liftEffect $ onKeyUp document (void <<< launchAff <<< game.query <<< H.action <<< Game.KeyboardEvent)

      pure unit
    Nothing ->
      pure unit

tickInterval :: forall o. (H.HalogenIO Game.Query o Aff) -> Effect Unit
tickInterval game = void $
  setInterval
      250
      (void $ launchAff $ game.query (H.action Game.Tick))

onKeyUp :: HTMLDocument -> (KeyboardEvent -> Effect Unit) -> Effect Unit
onKeyUp document fn = do
  listener <- ET.eventListener (traverse_ fn <<< KE.fromEvent)
  ET.addEventListener KET.keyup listener false (toEventTarget document)
