module Component.Game
  ( component
  , Query(..)
  ) where

import Prelude hiding (div)

import Data.Array (singleton)
import Data.Const (Const)
import Data.Either.Nested (type (\/))
import Data.Functor.Coproduct.Nested (type (<\/>))
import Data.Int (ceil, toNumber)
import Data.Maybe (Maybe(..), maybe)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Halogen as H
import Halogen.Component.ChildPath (cp1)
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

import Component.Inventory as Inventory
import Display (levelTiles, tilesRowElem, DisplayTile(..))
import Game (Game, tick)
import Level as Level
import Level (Tile(..), Color(..))
import Lib (getJSON)
import Utils (Direction(..))

-- | Accepts keyboard keypress events
data Query a
  = KeyboardEvent KeyboardEvent a
  | Tick a

type State = Game

type ChildSlot = Unit \/ Void
type ChildQuery = Inventory.Query <\/> Const Void

div :: forall p i. String -> Array (H.HTML p i) -> H.HTML p i
div classes = HH.div [ HP.class_ (H.ClassName classes) ]

dl :: forall a p i. Show a => String -> a -> H.HTML p i
dl term description =
  div "data-list"
    [ div "term" [ HH.text term ]
    , div "description" [ HH.text "888", div "value" [ HH.text (show description) ] ]
    ]

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
    div "game-container"
      [ div "content panel" (map tilesRowElem (levelTiles 4 level))
      , div "sidebar panel"
          [ HH.div_
              [ dl "LEVEL" levelNum
              , dl "TIME" (ceil ((toNumber ticksLeft) / 4.0 ))
              , dl "CHIPS LEFT" level.chipsLeft
              ]
          , renderInventory { inventory: level.inventory, hint: Level.visibleHint level }
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

renderInventory
  :: forall p i. { inventory :: Level.Inventory, hint :: Maybe String }
  -> H.HTML p i
renderInventory { inventory: { red, cyan, yellow, green }, hint } =
  div "inventory"
    -- [ HH.div_ (maybe [] (singleton <<< HH.text) hint)
    [ tilesRowElem
        [ if red > 0 then Tile (Key Red) else Floor
        , if cyan > 0 then Tile (Key Cyan) else Floor
        , if yellow > 0 then Tile (Key Yellow) else Floor
        , if green then Tile (Key Green) else Floor
        ]
    , tilesRowElem
        [ Floor
        , Floor
        , Floor
        , Floor
        ]
    ]
