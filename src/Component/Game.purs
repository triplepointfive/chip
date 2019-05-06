module Component.Game (gameComponent, Query(..)) where

import Component.Inventory as Inventory
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

data Query a
  = Move Direction a

type State = Level

data Slot = InventorySlot
derive instance eqInventorySlot :: Eq Slot
derive instance ordInventorySlot :: Ord Slot

gameComponent :: forall m. H.Component HH.HTML Query Unit Void m
gameComponent =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = buildLevel lvl1

  render :: State -> H.ParentHTML Query Inventory.Query Slot m
  render state =
    HH.div []
      [ HH.div [] (map tilesRowElem (levelTiles 4 state))
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Down)) ] [ HH.text "D" ]
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Left)) ] [ HH.text "L" ]
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Up)) ] [ HH.text "U" ]
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Right)) ] [ HH.text "R" ]
      , HH.slot InventorySlot Inventory.inventoryComponent state.inventory (const Nothing)
      ]

  eval :: Query ~> H.ParentDSL State Query Inventory.Query Slot Void m
  eval (Move direction next) = do
    H.modify_ (movePlayer direction)
    pure next

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
