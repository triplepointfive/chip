module Component.Keyboard (gameComponent, Query(..)) where

import Prelude

import Data.Foldable (traverse_)
import Effect (Effect)
import Web.Event.EventTarget as ET
import Web.HTML.HTMLDocument (HTMLDocument)
import Web.HTML.HTMLDocument as HTMLDocument
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent.EventTypes as KET

import Display
import Level
import Utils

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff (Aff)
import Prelude

import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Halogen.VDom.Driver (runUI)
import Web.Event.Event as E
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

type State = Level

data Query a
  = Init a
  | Move Direction a
  | HandleKey KeyboardEvent (H.SubscribeStatus -> a)

gameComponent :: forall m. H.Component HH.HTML Query Unit Void Aff
gameComponent =
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
  initialState = buildLevel lvl1

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div []
      [ HH.div [] (map tilesRowElem (levelTiles 4 state))
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Down)) ] [ HH.text "D" ]
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Left)) ] [ HH.text "L" ]
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Up)) ] [ HH.text "U" ]
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Right)) ] [ HH.text "R" ]
      , HH.div_ [HH.text (show state.inventory)]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void Aff
  eval (Init next) = do
    document <- H.liftEffect $ DOM.document =<< DOM.window
    H.subscribe $ ES.eventSource' (onKeyUp document) (Just <<< H.request <<< HandleKey)
    pure next
  eval (Move direction next) = do
    H.modify_ (movePlayer direction)
    pure next
  eval (HandleKey ev reply) = case KE.key ev of
      "ArrowDown"  -> modifyState (movePlayer Down)
      "ArrowLeft"  -> modifyState (movePlayer Left)
      "ArrowUp"    -> modifyState (movePlayer Up)
      "ArrowRight" -> modifyState (movePlayer Right)
      otherwise    -> pure (reply H.Listening)
    where
      modifyState f = do
        H.liftEffect $ E.preventDefault (KE.toEvent ev)
        H.modify_ f
        pure (reply H.Listening)

lvl1 :: Array String
lvl1 =
  [ "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "          ##### #####           "
  , "          #   ###   #           "
  , "          #   # #   #           "
  , "        #####G# #G#####         "
  , "        # y C     R y #         "
  , "        #   #c   r#   #         "
  , "        #####  @  #####         "
  , "        #   #c   r#   #         "
  , "        #   R     C   #         "
  , "        ######Y#Y######         "
  , "            #  #  #             "
  , "            #  #  #             "
  , "            #  #g #             "
  , "            #######             "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  ]


onKeyUp :: HTMLDocument -> (KeyboardEvent -> Effect Unit) -> Effect (Effect Unit)
onKeyUp document fn = do
  let target = HTMLDocument.toEventTarget document
  listener <- ET.eventListener (traverse_ fn <<< KE.fromEvent)
  ET.addEventListener KET.keyup listener false target
  pure $ ET.removeEventListener KET.keyup listener false target
