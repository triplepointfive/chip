module Component.Game
  ( component
  , Query(..)
  ) where

import Prelude

import Data.Either.Nested (type (\/))
import Data.Const (Const)
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Component.ChildPath (cp1, cp2)
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

import Component.Inventory as Inventory
import Component.Keyboard as Keyboard
import Display (levelTiles, tilesRowElem)
import Level as Level
import Utils (Direction(..))

-- | Accepts keyboard keypress events
data Query a
  = Move Direction a
  | KeyboardEvent Keyboard.Message a

type State = Level.Level

type ChildSlot = Unit \/ Unit \/ Void
type ChildQuery = Inventory.Query <\/> Keyboard.Query <\/> Const Void

-- | Top game component
component :: H.Component HH.HTML Query Unit Void Aff
component =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = Level.build lvl1

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render state =
    HH.div_
      [ HH.div [] (map tilesRowElem (levelTiles 4 state))
      , HH.div_ [ HH.text (show state.chipsLeft) ]
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Down)) ] [ HH.text "D" ]
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Left)) ] [ HH.text "L" ]
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Up)) ] [ HH.text "U" ]
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Right)) ] [ HH.text "R" ]
      , HH.slot'
          cp1
          unit
          Inventory.component
          { inventory: state.inventory, hint: Level.visibleHint state }
          absurd
      , HH.slot'
          cp2
          unit
          Keyboard.component
          unit
          (HE.input KeyboardEvent)
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
  eval (KeyboardEvent (Keyboard.Move direction) next) = do
    H.modify_ (Level.movePlayer direction)
    pure next
  eval (Move direction next) = do
    H.modify_ (Level.movePlayer direction)
    pure next

lvl1 :: Level.Blank
lvl1 =
  { grid:
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
      , "          # + #<# + #           "
      , "        #####G#-#G#####         "
      , "        # y C     R y #         "
      , "        # + #c ? r# + #         "
      , "        #####+ @ +#####         "
      , "        # + #c   r# + #         "
      , "        #   R  +  C   #         "
      , "        ######Y#Y######         "
      , "            #  #  #             "
      , "            # +#+ #             "
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
  , hint: Just "Collect chips to get past the chip socket. Use keys to open doors."
  , name: "LESSON 1"
  , chips: 11
  }
