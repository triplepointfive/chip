module Component.Keyboard (component, Query(..), Message(..)) where

import Utils (Direction(..))

import Data.Maybe (Maybe(..))
import Data.Foldable (traverse_)
import Effect (Effect)
import Effect.Aff (Aff)
import Prelude (type (~>), Unit, bind, const, discard, pure, unit, ($), (<<<), (=<<))

import Halogen as H
import Halogen.HTML as HH
import Halogen.Query.EventSource as ES
import Web.Event.Event as E
import Web.Event.EventTarget as ET
import Web.HTML (window) as DOM
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.HTML.Window (document) as DOM
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent.EventTypes as KET

type State = Unit

data Query a
  = Init a
  | HandleKey KeyboardEvent (H.SubscribeStatus -> a)

data Message = Move Direction

component :: H.Component HH.HTML Query Unit Message Aff
component =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = unit

  render :: State -> H.ComponentHTML Query
  render = const $ HH.div_ []

  eval :: Query ~> H.ComponentDSL State Query Message Aff
  eval (Init next) = do
    document <- H.liftEffect $ DOM.document =<< DOM.window
    H.subscribe $ ES.eventSource' (onKeyUp document) (Just <<< H.request <<< HandleKey)
    pure next
  eval (HandleKey ev reply) = case KE.key ev of
      "ArrowDown"  -> raiseMoveEvent Down
      "ArrowLeft"  -> raiseMoveEvent Left
      "ArrowUp"    -> raiseMoveEvent Up
      "ArrowRight" -> raiseMoveEvent Right
      otherwise    -> pure (reply H.Listening)
    where
      raiseMoveEvent dir = do
        H.liftEffect $ E.preventDefault (KE.toEvent ev)
        H.raise $ Move dir
        pure (reply H.Listening)

onKeyUp :: HTMLDocument -> (KeyboardEvent -> Effect Unit) -> Effect (Effect Unit)
onKeyUp document fn = do
  let target = HTMLDocument.toEventTarget document
  listener <- ET.eventListener (traverse_ fn <<< KE.fromEvent)
  ET.addEventListener KET.keyup listener false target
  pure $ ET.removeEventListener KET.keyup listener false target
