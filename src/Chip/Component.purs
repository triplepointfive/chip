module Chip.Component
  ( component
  , Action'(..)
  , Query(..)
  , processAction
  , ticksPerSecond
  ) where

import Prelude hiding (div)

import Data.Int (ceil, odd, toNumber)
import Data.Maybe (Maybe(..), isJust)
import Data.Map as Map
import Effect.Aff (Aff)
import Effect.Aff.Class (class MonadAff)
import Control.Monad.State (class MonadState)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP
import Web.UIEvent.KeyboardEvent as KE
import Web.UIEvent.KeyboardEvent (KeyboardEvent)

import Chip.Action (Action(..), DieReason(..), ActionResult, Sound(..))
import Chip.Action.AI (actAI)
import Chip.Action.Tick (tick)
import Display (levelTiles, tilesRowElem, DisplayTile(..))
import Chip.Game (Game)
import Chip.Game as Game
import Level as Level
import Chip.Level.Build (Blank, build)
import Chip.Lib (getJSON)
import Chip.Sound (SoundEffect(..), play)
import Chip.Tile (Color(..), Item(..), Tile(..))
import Chip.Utils (Direction(..), Point, foldlM)

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

  builtLevel = build initBlank

  initialState :: State
  initialState =
    { level: builtLevel
    , levelNum: initLevelNum
    , name: initBlank.name
    , state: Game.Init
    , intactLevel: builtLevel
    , moving: Game.Unpressed
    }

  render game =
    div "game-container"
      [ div "content"
          ( renderMessage game
          <> map tilesRowElem (levelTiles 16 game)
          )
      , renderSidebar game
      ]

handleQuery :: forall a. Query a -> H.HalogenM State Action () Message Aff (Maybe a)
handleQuery (KeyboardUp ev next) = do
  { moving } <- H.get

  case moving of
    Game.Pressed dir -> do
      H.modify_ (_ { moving = Game.Released dir })
      pure (Just next)
    Game.Processed dir -> do
      H.modify_ (_ { moving = Game.Unpressed })
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

    H.modify_ (_ { moving = Game.Pressed direction })

    pure (Just next)

handleQuery (Tick next) = do
  { moving, state, level: { ticksLeft, tiles, player: { movedAt, pos } } } <- H.get

  case state of
    Game.Init -> pure (Just next)
    Game.Dead _ -> pure (Just next)
    _ -> do
      runAction tick

      when (odd ticksLeft) (runAction (Level.checkForEnemies <<< actAI))

      when
        (moveAction tiles pos movedAt ticksLeft)
        (moveTo moving)

      runAction Level.slide

      pure (Just next)

moveTo :: forall m. Bind m => MonadState Game m => MonadAff m => Game.Moving -> m Unit
moveTo moving = case moving of
  Game.Released direction -> do
      H.modify_ (_ { moving = Game.Unpressed })
      runAction (Level.movePlayer true direction)
  Game.Pressed direction -> do
      H.modify_ (_ { moving = Game.Processed direction })
      runAction (Level.movePlayer true direction)
  Game.Processed direction ->
      runAction (Level.movePlayer true direction)
  Game.Unpressed -> pure unit

moveAction :: Level.Tiles -> Point -> Int -> Int -> Boolean
moveAction tiles pos movedAt ticksLeft = case Map.lookup pos tiles of
      Just (Force _) -> true
      _ | movedAt - ticksLeft >= 1 -> true
      _ -> false

runAction
  :: forall m. Bind m
  => MonadAff m
  => MonadState Game m
  => (Level.Level -> ActionResult Level.Level)
  -> m Unit
runAction f = do
  game <- H.get
  when (Game.notDead game) do
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
