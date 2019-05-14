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
import Game (Game, onLevel)
import Level as Level

-- | Accepts keyboard keypress events
data Query a
  = KeyboardEvent Keyboard.Message a

type State = Game

type ChildSlot = Unit \/ Unit \/ Void
type ChildQuery = Inventory.Query <\/> Keyboard.Query <\/> Const Void

-- | Top game component
component :: Level.Blank -> Int -> H.Component HH.HTML Query Unit Void Aff
component blank levelNum =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = { level: Level.build blank, levelNum }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render { level } =
    HH.div [ HP.class_ (H.ClassName "game-container") ]
      [ HH.div
          [ HP.class_ (H.ClassName "content") ]
          (map tilesRowElem (levelTiles 4 level))
      , HH.div
          [ HP.class_ (H.ClassName "sidebar") ]
          [ HH.div_ [ HH.text (show level.chipsLeft) ]
          , HH.slot'
              cp1
              unit
              Inventory.component
              { inventory: level.inventory, hint: Level.visibleHint level }
              absurd
          , HH.slot'
              cp2
              unit
              Keyboard.component
              unit
              (HE.input KeyboardEvent)
          ]
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
  eval (KeyboardEvent (Keyboard.Move direction) next) = do
    H.modify_ $ onLevel (Level.movePlayer direction)
    pure next
