module Component.Game
  ( component
  , Query(..)
  ) where

import Prelude

import Data.Either.Nested (type (\/))
import Data.Const (Const)
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Int (ceil, toNumber)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Component.ChildPath (cp1)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

import Component.Inventory as Inventory
import Display (levelTiles, tilesRowElem)
import Game (Game, tick)
import Level as Level
import Lib (getJSON)
import Utils (Direction(..))

-- | Accepts keyboard keypress events
data Query a
  = KeyboardEvent KeyboardEvent a
  | Tick a

type State = Game

type ChildSlot = Unit \/ Void
type ChildQuery = Inventory.Query <\/> Const Void

-- | Top game component
component :: Level.Blank -> Int -> H.Component HH.HTML Query Unit Void Aff
component initBlank initLevelNum =
  H.parentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState =
    { level: Level.build initBlank
    , levelNum: initLevelNum
    , ticksLeft: 100 * 4
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render { level, levelNum, ticksLeft } =
    HH.div [ HP.class_ (H.ClassName "game-container") ]
      [ HH.div
          [ HP.class_ (H.ClassName "panel content") ]
          (map tilesRowElem (levelTiles 4 level))
      , HH.div
          [ HP.class_ (H.ClassName "panel sidebar") ]
          [ HH.div
              [ HP.class_ (H.ClassName "data-list") ]
              [ HH.div [ HP.class_ (H.ClassName "term") ] [ HH.text "LEVEL" ]
              , HH.div [ HP.class_ (H.ClassName "description") ] [ HH.text (show levelNum) ]
              ]
          , HH.div
              [ HP.class_ (H.ClassName "data-list") ]
              [ HH.div [ HP.class_ (H.ClassName "term") ] [ HH.text "TIME" ]
              , HH.div
                  [ HP.class_ (H.ClassName "description") ]
                  [ HH.text (show $ ceil ((toNumber ticksLeft) / 4.0 )) ]
              ]
          , HH.slot'
              cp1
              unit
              Inventory.component
              { inventory: level.inventory, hint: Level.visibleHint level }
              absurd
          ]
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
  eval (KeyboardEvent ev next) = case KE.key ev of
    "ArrowDown"  -> raiseMoveEvent Down
    "ArrowLeft"  -> raiseMoveEvent Left
    "ArrowUp"    -> raiseMoveEvent Up
    "ArrowRight" -> raiseMoveEvent Right
    otherwise    -> pure next

    where

    raiseMoveEvent direction = do
      game <- H.get
      let Tuple level actions = Level.movePlayer direction game.level
      H.put (game { level = level })

      case actions of
        [Level.CompleteLevel] -> do
          result <- H.liftAff $ getJSON ("levels/" <> show (game.levelNum + 1) <> ".json")
          case result of
            Just blank -> do
              H.put (game { level = Level.build blank, levelNum = game.levelNum + 1 })
              pure next
            Nothing ->
              pure next
        _ -> do
          pure next

  eval (Tick next) = do
    H.modify_ (tick)
    pure next
