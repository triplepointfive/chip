module Main where

import Level
import Keyboard as K

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

type Input = Unit

type Message = Void

tileClasses :: Tile -> Array String
tileClasses Floor = ["tile", "-floor"]
tileClasses Wall = ["tile", "-wall"]
tileClasses (Boy Down) = ["tile", "-boy", "-down"]
tileClasses (Boy Left) = ["tile", "-boy", "-left"]
tileClasses (Boy Up) = ["tile", "-boy", "-up"]
tileClasses (Boy Right) = ["tile", "-boy", "-right"]

tileToElem :: forall p i. Tile -> HH.HTML p i
tileToElem tile = HH.span [ HP.classes $ map H.ClassName (tileClasses tile) ] []

tilesRowElem :: forall p i. Array Tile -> HH.HTML p i
tilesRowElem tiles =
  HH.div
    [ HP.class_ (H.ClassName "level-row") ]
    (map tileToElem tiles)

mainComponent :: forall m. H.Component HH.HTML Query Input Message Aff
mainComponent =
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
      ]

  eval :: Query ~> H.ComponentDSL State Query Message Aff
  eval (Init next) = do
    document <- H.liftEffect $ DOM.document =<< DOM.window
    H.subscribe $ ES.eventSource' (K.onKeyUp document) (Just <<< H.request <<< HandleKey)
    pure next
  eval (Move direction next) = do
    state <- H.get
    let nextState = movePlayer direction state
    H.put nextState
    -- H.raise $ Toggled nextState
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

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI mainComponent unit body

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
  , "        ##### # # #####         "
  , "        #             #         "
  , "        #   #     #   #         "
  , "        #####  @  #####         "
  , "        #   #     #   #         "
  , "        #             #         "
  , "        ###### # ######         "
  , "            #  #  #             "
  , "            #  #  #             "
  , "            #  #  #             "
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
