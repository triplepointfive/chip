module Component.Game
  ( component
  , Query(..)
  , processAction
  , ticksPerSecond
  ) where

import Prelude hiding (div)

import Data.Const (Const)
import Data.Int (ceil, even, toNumber)
import Data.Maybe (Maybe(..), isJust)
import Data.Tuple (Tuple(..))
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

import Chip.Tile (Color(..), Item(..), Tile(..))
import Display (levelTiles, tilesRowElem, DisplayTile(..))
import Game (Game, tick)
import Game as Game
import Level as Level
import Lib (getJSON)
import Utils (Direction(..), foldlM)

ticksPerSecond :: Int
ticksPerSecond = 8

-- | Accepts keyboard keypress events
data Query a
  = KeyboardEvent KeyboardEvent a
  | Tick a

type State = Game

type ChildSlot = Void
type ChildQuery = Const Void

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
    , ticksLeft: initBlank.timeLimit * ticksPerSecond
    , name: initBlank.name
    , state: Game.Init
    }

  render :: State -> H.ParentHTML Query ChildQuery ChildSlot Aff
  render game =
    div "game-container"
      [ div "content"
          ( renderMessage game
          <> map tilesRowElem (levelTiles 7 game)
          )
      , renderSidebar game
      ]

  eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void Aff
  eval (KeyboardEvent ev next) = do
    { state } <- H.get

    case state of
      Game.Dead _ -> pure next
      _ -> case KE.key ev of
          "ArrowDown"  -> raiseMoveEvent Down
          "ArrowLeft"  -> raiseMoveEvent Left
          "ArrowUp"    -> raiseMoveEvent Up
          "ArrowRight" -> raiseMoveEvent Right
          otherwise    -> pure next

    where

    raiseMoveEvent direction = do
      { state } <- H.get
      when (state == Game.Init) (H.modify_ (_ { state = Game.Alive }))

      game <- H.get
      let Tuple level actions = Level.movePlayer true direction game.level

      foldlM processAction (game { level = level }) actions >>= H.put
      pure next

  eval (Tick next) = do
    { state, ticksLeft } <- H.get

    case state of
      Game.Init -> pure next
      Game.Dead _ -> pure next
      _ -> do
        when (ticksLeft == 1) (H.modify_ (_ { state = Game.Dead Level.Timed }))
        H.modify_ tick

        when (even ticksLeft) $ do
          game <- H.get
          let Tuple level actions = Level.enemyAct game.level
          foldlM processAction (game { level = level }) actions >>= H.put

        game <- H.get
        let Tuple level actions = Level.slide game.level
        foldlM processAction (game { level = level }) actions >>= H.put

        pure next

processAction :: forall m. Bind m => MonadAff m => Game -> Level.Action -> m Game
processAction game = case _ of
  Level.Complete -> do
      result <- H.liftAff $ getJSON ("levels/" <> show (game.levelNum + 1) <> ".json")
      case result of
        Just blank -> pure $ game
            { level = Level.build blank
            , levelNum = game.levelNum + 1
            , name = blank.name
            , ticksLeft = blank.timeLimit * ticksPerSecond
            , state = Game.Init
            }
        Nothing -> pure game
  Level.Die reason -> pure (game { state = Game.Dead reason })

dieMessage :: Level.DieReason -> String
dieMessage = case _ of
  Level.Drown -> "Ooops! Chip can't swim without flippers!"
  Level.Burned -> "Ooops! Don't step in the fire without fire boots!"
  Level.Eaten -> "Ooops! Look out for creatures!"
  Level.Timed -> "Ooops! Don't step in the fire without fire boots!"

renderMessage :: forall p i. Game -> Array (H.HTML p i)
renderMessage { state, name } = case state of
  Game.Init -> [ div "modal -level" [ HH.text name ] ]
  Game.Dead reason -> [ div "modal -dead" [ HH.text (dieMessage reason), HH.br_, HH.text "Press R to restart" ] ]
  _ -> []

renderSidebar :: forall p i. Game -> H.HTML p i
renderSidebar { level, levelNum, ticksLeft } =
  div "sidebar"
    [ HH.div_ $ (
        [ dl "LEVEL" levelNum
        , dl "TIME" (ceil ((toNumber ticksLeft) / (toNumber ticksPerSecond) ))
        ]
        <> (if isJust hint then [] else [dl "CHIPS LEFT" level.chipsLeft])
        )
    , case hint of
      Just msg -> div "hint" [ HH.text msg ]
      Nothing -> div "inventory"
        [ tilesRowElem
            [ if level.inventory.red > 0 then Tile (Item (Key Red)) else Floor
            , if level.inventory.cyan > 0 then Tile (Item (Key Cyan)) else Floor
            , if level.inventory.yellow > 0 then Tile (Item (Key Yellow)) else Floor
            , if level.inventory.green then Tile (Item (Key Green)) else Floor
            ]
        , tilesRowElem
            [ if level.inventory.skiSkates then Tile (Item SkiSkates) else Floor
            , if level.inventory.suctionBoots then Tile (Item SuctionBoots) else Floor
            , if level.inventory.fireBoots then Tile (Item FireBoots) else Floor
            , if level.inventory.flippers then Tile (Item Flippers) else Floor
            ]
        ]
    ]

  where

  hint = Level.visibleHint level
