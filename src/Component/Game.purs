module Component.Game
  ( component
  , Action'(..)
  , Query(..)
  , processAction
  , ticksPerSecond
  ) where

import Prelude hiding (div)

import Data.Int (ceil, even, toNumber)
import Data.Maybe (Maybe(..), isJust)
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Control.Monad.State (class MonadState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

import Chip.Action (Action(..), DieReason(..), ActionResult)
import Chip.Action.AI (actAI)
import Chip.Action.Tick (tick)
import Chip.Level.Build ( Blank, build)
import Chip.Tile (Color(..), Item(..), Tile(..))
import Display (levelTiles, tilesRowElem, DisplayTile(..))
import Game (Game)
import Game as Game
import Level as Level
import Chip.Lib (getJSON)
import Chip.Utils (Direction(..), foldlM)

ticksPerSecond :: Int
ticksPerSecond = 8

type Action' = Void

type Message = Void
data Query a
  = KeyboardEvent KeyboardEvent a
  | Tick a
type State = Game

div :: forall p i. String -> Array (HH.HTML p i) -> HH.HTML p i
div classes = HH.div [ HP.class_ (H.ClassName classes) ]

dl :: forall a p i. Show a => String -> a -> HH.HTML p i
dl term description =
  div "data-list"
    [ div "term" [ HH.text term ]
    , div "description" [ HH.text "888", div "value" [ HH.text (show description) ] ]
    ]

-- | Top game component
component :: Blank -> Int -> H.Component HH.HTML Query Unit Message Aff
component initBlank initLevelNum =
  H.mkComponent
    { initialState: const initialState
    , render
    , eval: H.mkEval $ H.defaultEval { handleQuery = handleQuery }
    }
  where

  initialState :: State
  initialState =
    { level: build initBlank
    , levelNum: initLevelNum
    , name: initBlank.name
    , state: Game.Init
    }

  render game =
    div "game-container"
      [ div "content"
          ( renderMessage game
          <> map tilesRowElem (levelTiles 7 game)
          )
      , renderSidebar game
      ]

handleQuery :: forall a. Query a -> H.HalogenM State Action () Message Aff (Maybe a)
handleQuery (KeyboardEvent ev next) = do
  { state } <- H.get

  case state of
    Game.Dead _ -> pure (Just next)
    _ -> case KE.key ev of
        "ArrowDown"  -> raiseMoveEvent Down
        "ArrowLeft"  -> raiseMoveEvent Left
        "ArrowUp"    -> raiseMoveEvent Up
        "ArrowRight" -> raiseMoveEvent Right
        otherwise    -> pure (Just next)

  where

  raiseMoveEvent direction = do
    { state } <- H.get
    when (state == Game.Init) (H.modify_ (_ { state = Game.Alive }))

    game <- H.get
    let { result: level, actions } = Level.movePlayer true direction game.level

    foldlM processAction (game { level = level }) actions >>= H.put
    pure (Just next)

handleQuery (Tick next) = do
  { state, level: { ticksLeft } } <- H.get

  case state of
    Game.Init -> pure (Just next)
    Game.Dead _ -> pure (Just next)
    _ -> do
      runAction tick

      when (even ticksLeft) (runAction (Level.checkForEnemies <<< actAI))

      runAction Level.slide

      pure (Just next)

runAction
  :: forall m. Bind m
  => MonadAff m
  => MonadState Game m
  => (Level.Level -> ActionResult Level.Level)
  -> m Unit
runAction f = do
  game <- H.get
  let { result: level, actions } = f game.level
  foldlM processAction (game { level = level }) actions >>= H.put

processAction :: forall m. Bind m => MonadAff m => Game -> Action -> m Game
processAction game = case _ of
  Complete -> do
      result <- H.liftAff $ getJSON ("levels/" <> show (game.levelNum + 1) <> ".json")
      case result of
        Just blank -> pure $ game
            { level = build blank
            , levelNum = game.levelNum + 1
            , name = blank.name
            , state = Game.Init
            }
        Nothing -> pure game
  Die reason -> pure (game { state = Game.Dead reason })

dieMessage :: DieReason -> String
dieMessage = case _ of
  Drown -> "Ooops! Chip can't swim without flippers!"
  Burned -> "Ooops! Don't step in the fire without fire boots!"
  Eaten -> "Ooops! Look out for creatures!"
  Timed -> "Ooops! Out of time!"
  BlownUp -> "Ooops! Don't touch the bombs!"

renderMessage :: forall p i. Game -> Array (HH.HTML p i)
renderMessage { state, name } = case state of
  Game.Init -> [ div "modal -level" [ HH.text name ] ]
  Game.Dead reason -> [ div "modal -dead" [ HH.text (dieMessage reason), HH.br_, HH.text "Press R to restart" ] ]
  _ -> []

renderSidebar :: forall p i. Game -> HH.HTML p i
renderSidebar { level, levelNum } =
  div "sidebar"
    [ HH.div_ $ (
        [ dl "LEVEL" levelNum
        , dl "TIME" (ceil ((toNumber level.ticksLeft) / (toNumber ticksPerSecond) ))
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
