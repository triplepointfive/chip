module Chip.Component
  ( component
  , Action'(..)
  , Query(..)
  , processAction
  , ticksPerSecond
  ) where

import Prelude hiding (div)

import Data.Int (ceil, odd, toNumber)
import Data.Maybe (Maybe(..), isNothing, maybe)
import Data.Map as Map
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Control.Monad.State (class MonadState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

import Chip.Action (Action(..), ActionResult, Sound(..))
import Chip.Action.AI (actAI)
import Chip.Action.Tick as Action
import Display (levelTiles, tilesRowElem, DisplayTile(..))
import Chip.Game (Game, notDead, Moving(..))
import Level as Level
import Chip.Level.Build (Blank, build)
import Chip.Lib (getJSON)
import Chip.Model (DieReason(..), Point, Level, Tiles, Color(..), Item(..), Tile(..), Direction(..))
import Chip.Model (State(..)) as Game
import Chip.Sound (SoundEffect(..), play)
import Chip.Utils (foldlM)

ticksPerSecond :: Int
ticksPerSecond = 10

type Action' = Void

type Message = Void
data Query a
  = KeyboardDown KeyboardEvent a
  | KeyboardUp KeyboardEvent a
  | Tick a
type State = Game

div :: forall p i. String -> Array (HH.HTML p i) -> HH.HTML p i
div classes = HH.div [ HP.class_ (H.ClassName classes) ]

dl :: forall a p i. Show a => String -> Maybe a -> HH.HTML p i
dl term description =
  div "data-list"
    [ div "term" [ HH.text term ]
    , div "description"
        [ HH.text "888"
        , div (if isNothing description then "value -low" else "value")
              [ HH.text (maybe "---" show description) ]
        ]
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

  builtLevel = build initBlank

  initialState :: State
  initialState =
    { level: builtLevel
    , levelNum: initLevelNum
    , name: initBlank.name
    , state: Game.Init
    , intactLevel: builtLevel
    , moving: Unpressed
    }

  render game =
    div "game-container"
      [ div "content"
          ( renderMessage game
          <> map tilesRowElem (levelTiles 18 game)
          )
      , renderSidebar game
      ]

handleQuery :: forall a. Query a -> H.HalogenM State Action () Message Aff (Maybe a)
handleQuery (KeyboardUp ev next) = do
  { moving } <- H.get

  case moving of
    Pressed dir -> do
      H.modify_ (_ { moving = Released dir })
      pure (Just next)
    Processed dir -> do
      H.modify_ (_ { moving = Unpressed })
      pure (Just next)
    _ ->
      pure (Just next)

handleQuery (KeyboardDown ev next) = do
  { state, intactLevel, levelNum } <- H.get

  case { state, key: KE.key ev } of
    { key: "r" } -> do
        H.modify_ (_ { level = intactLevel, state = Game.Init } )
        pure (Just next)
    { state: Game.Dead _ } -> pure (Just next)
    { key: " ", state: Game.Pause } -> do
        H.modify_ (_ { state = Game.Alive })
        pure (Just next)
    { key: " " } -> do
        H.modify_ (_ { state = Game.Pause })
        pure (Just next)
    { state: Game.Pause } -> pure (Just next)
    { key: "s" } -> raiseMoveEvent Down
    { key: "a" } -> raiseMoveEvent Left
    { key: "w" } -> raiseMoveEvent Up
    { key: "d" } -> raiseMoveEvent Right
    { key: "ArrowDown" } -> raiseMoveEvent Down
    { key: "ArrowLeft" } -> raiseMoveEvent Left
    { key: "ArrowUp" } -> raiseMoveEvent Up
    { key: "ArrowRight" } -> raiseMoveEvent Right
    { key: "n" } -> do
        H.get >>= loadLevel (levelNum + 1) >>= H.put
        pure (Just next)
    { key: "p" } | levelNum > 1 -> do
        H.get >>= loadLevel (levelNum - 1) >>= H.put
        pure (Just next)
    _ -> pure (Just next)

  where

  raiseMoveEvent direction = do
    { state, level: { ticksLeft, player: { movedAt } } } <- H.get

    when (state == Game.Init) (H.modify_ (_ { state = Game.Alive }))

    H.modify_ (_ { moving = Pressed direction })

    pure (Just next)

handleQuery (Tick next) = do
  { moving, state, level: { tick, tiles, player: { movedAt, pos } } } <- H.get

  case state of
    Game.Init -> pure (Just next)
    Game.Dead _ -> pure (Just next)
    Game.Pause -> pure (Just next)
    _ -> do
      runAction Action.tick

      when (odd tick) (runAction (Level.checkForEnemies <<< actAI))

      when
        (moveAction tiles pos movedAt tick)
        (moveTo moving)

      runAction Level.slide

      pure (Just next)

moveTo :: forall m. Bind m => MonadState Game m => MonadAff m => Moving -> m Unit
moveTo moving = case moving of
  Released direction -> do
      H.modify_ (_ { moving = Unpressed })
      runAction (Level.movePlayer true direction)
  Pressed direction -> do
      H.modify_ (_ { moving = Processed direction })
      runAction (Level.movePlayer true direction)
  Processed direction ->
      runAction (Level.movePlayer true direction)
  Unpressed -> pure unit

moveAction :: Tiles -> Point -> Int -> Int -> Boolean
moveAction tiles pos movedAt tick = case Map.lookup pos tiles of
      Just (Force _) -> true
      _ | tick - movedAt >= 1 -> true
      _ -> false

runAction
  :: forall m. Bind m
  => MonadAff m
  => MonadState Game m
  => (Level -> ActionResult Level)
  -> m Unit
runAction f = do
  game <- H.get
  when (notDead game) do
    let { result: level, actions } = f game.level
    foldlM processAction (game { level = level }) actions >>= H.put

loadLevel :: forall m. Bind m => MonadAff m => Int -> Game -> m Game
loadLevel number game = do
  result <- H.liftAff $ getJSON ("levels/" <> show number <> ".json")
  case result of
    Just blank -> let builtLevel = build blank in pure $ game
        { level = builtLevel
        , levelNum = number
        , name = blank.name
        , state = Game.Init
        , intactLevel = builtLevel
        }
    Nothing -> pure game

processAction :: forall m. Bind m => MonadAff m => Game -> Action -> m Game
processAction game = case _ of
  Complete -> do
      _ <- processAction game (PlaySound LevelComplete)
      loadLevel (game.levelNum + 1) game

  Die reason ->
      _ { state = Game.Dead reason } <$> processAction game (PlaySound Bummer)

  PlaySound sound -> do
      H.liftEffect $ play (Sound (soundFile sound) 1.0)
      pure game

soundFile :: Sound -> String
soundFile sound = "sounds/" <> case sound of
  PickUpChip -> "CLICK3.WAV"
  Oof -> "OOF3.WAV"
  DoorOpen -> "DOOR.WAV"
  PickUpItem -> "BLIP2.WAV"
  Bummer -> "BUMMER.WAV"
  LevelComplete -> "DITTY1.WAV"
  Steal -> "STRIKE.WAV"
  Teleported -> "TELEPORT.WAV"
  Splash -> "WATER2.WAV"

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
  Game.Pause -> [ div "modal -pause" [ HH.text "Pause" ] ]
  Game.Dead reason -> [ div "modal -dead" [ HH.text (dieMessage reason), HH.br_, HH.text "Press R to restart" ] ]
  _ -> []

renderSidebar :: forall p i. Game -> HH.HTML p i
renderSidebar { level, levelNum } =
  div "sidebar"
    [ HH.div_ $ (
        [ dl "LEVEL" (Just levelNum)
        , dl "TIME" (toSeconds <$> level.ticksLeft)
        , dl "CHIPS LEFT" (Just level.chipsLeft)
        ]
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
  toSeconds ticksLeft = ceil ((toNumber ticksLeft) / (toNumber ticksPerSecond) )
