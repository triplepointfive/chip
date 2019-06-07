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
import Web.Event.Event (EventType)
import Web.Event.EventTarget as ET
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import Web.HTML.HTMLDocument (HTMLDocument, toEventTarget)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import Chip.Component as Game
import Chip.Lib (getJSON)

-- | Outputs main game component
main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  result <- getJSON "levels/3.json"
  case result of
    Just blank -> do
      game <- runUI (Game.component blank 1) unit body
      liftEffect $ tickInterval game

      document <- H.liftEffect $ DOM.document =<< DOM.window
      liftEffect $ do
          on KET.keyup document (game.query <<< H.tell <<< Game.KeyboardUp)
          on KET.keydown document (game.query <<< H.tell <<< Game.KeyboardDown)

      pure unit
    Nothing ->
      pure unit

tickInterval :: forall o. (H.HalogenIO Game.Query o Aff) -> Effect Unit
tickInterval game = void $
  setInterval
      (1000 / Game.ticksPerSecond)
      (void $ launchAff $ game.query (H.tell Game.Tick))

on :: forall a. EventType -> HTMLDocument -> (KeyboardEvent -> Aff a) -> Effect Unit
on eventType document fn = do
  listener <- ET.eventListener (traverse_ (void <<< launchAff <<< fn) <<< KE.fromEvent)
  ET.addEventListener eventType listener false (toEventTarget document)
