module Display
  ( DisplayTile(..)
  , levelTiles
  , tilesRowElem
  ) where

import Prelude

import Data.Array (range)
import Data.Map (lookup)
import Data.Maybe (maybe, Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Chip.Action (DieReason(..))
import Chip.Enemy (Enemy(..))
import Chip.Tile (Color(..), Tile(..), Item(..))
import Level (mapSize)
import Game (Game, State(..))
import Utils (Direction(..), Point, SwitchState(..))

-- | What shall be output on screen
data DisplayTile
  = Floor
  | Tile Tile
  | Boy Direction
  | Creature Enemy
  | Swimming Direction
  | DrownBoy
  | BurnedBoy
  | Block

-- | Builds a matrix of `DisplayTile` with `radius` * 2 + 1 size.
-- | This function transforms raw level data to what shall be
-- | presented in specific cell
levelTiles :: Int -> Game -> Array (Array DisplayTile)
levelTiles radius game =
  map
    (\(Tuple y row) -> map (\x -> buildTile { x: x, y: y } game) row)
    (rangeSlice radius game.level.player.pos)

rangeSlice :: Int -> Point ->  Array (Tuple Int (Array Int))
rangeSlice r { x, y } =
  map (flip Tuple xRange) (range (from y) (d + from y - 1))

  where

  xRange = range (from x) (d + from x - 1)
  d = (2 * r) + 1
  from c = min (max 0 (c - r)) (mapSize - d)

tileClasses :: DisplayTile -> String
tileClasses = case _ of
  Floor -> "tile -floor"
  Tile Wall -> "tile -wall"
  Tile Chip -> "tile -chip"
  Tile (Door Red) -> "tile -door -red"
  Tile (Door Cyan) -> "tile -door -cyan"
  Tile (Door Yellow) -> "tile -door -yellow"
  Tile (Door Green) -> "tile -door -green"
  Tile (Force Down) -> "tile -force -down"
  Tile (Force Left) -> "tile -force -left"
  Tile (Force Up) -> "tile -force -up"
  Tile (Force Right) -> "tile -force -right"
  Boy Down -> "tile -boy -down"
  Boy Left -> "tile -boy -left"
  Boy Up -> "tile -boy -up"
  Boy Right -> "tile -boy -right"
  Creature (Bee Down) -> "tile -bee -down"
  Creature (Bee Left) -> "tile -bee -left"
  Creature (Bee Up) -> "tile -bee -up"
  Creature (Bee Right) -> "tile -bee -right"
  Creature (Glider Down) -> "tile -glider -down"
  Creature (Glider Left) -> "tile -glider -left"
  Creature (Glider Up) -> "tile -glider -up"
  Creature (Glider Right) -> "tile -glider -right"
  Creature (Tank Down) -> "tile -tank -down"
  Creature (Tank Left) -> "tile -tank -left"
  Creature (Tank Up) -> "tile -tank -up"
  Creature (Tank Right) -> "tile -tank -right"
  Creature (Ball _) -> "tile -ball"
  Creature (FireBall _) -> "tile -fireball"
  Tile (CloneMachine _) -> "tile -clone-machine"
  Block -> "tile -block"
  Swimming Down -> "tile -boy -down -swimming"
  Swimming Left -> "tile -boy -left -swimming"
  Swimming Up -> "tile -boy -up -swimming"
  Swimming Right -> "tile -boy -right -swimming"
  Tile Bomb -> "tile -bomb"
  Tile Water -> "tile -water"
  Tile Fire -> "tile -fire"
  Tile Ice -> "tile -ice"
  Tile (IceCorner Down) -> "tile -ice -down"
  Tile (IceCorner Left) -> "tile -ice -left"
  Tile (IceCorner Up) -> "tile -ice -up"
  Tile (IceCorner Right) -> "tile -ice -right"
  Tile (Item (Key Red)) -> "tile -key -red"
  Tile (Item (Key Cyan)) -> "tile -key -cyan"
  Tile (Item (Key Yellow)) -> "tile -key -yellow"
  Tile (Item (Key Green)) -> "tile -key -green"
  Tile (Item SkiSkates) -> "tile -ski-skates"
  Tile (Item SuctionBoots) -> "tile -suction-boots"
  Tile (Item FireBoots) -> "tile -fire-boots"
  Tile (Item Flippers) -> "tile -flippers"
  Tile (SwitchableWall On) -> "tile -switchable-wall -on"
  Tile (SwitchableWall Off) -> "tile -switchable-wall -off"
  Tile WallButton -> "tile -wall-button"
  Tile TankButton -> "tile -tank-button"
  Tile CloneMachineButton -> "tile -clone-machine-button"
  Tile Exit -> "tile -exit"
  Tile Dirt -> "tile -dirt"
  Tile Hint -> "tile -hint"
  Tile Socket -> "tile -socket"
  Tile (Trap _) -> "tile -trap"
  Tile TrapButton -> "tile -trap-button"
  DrownBoy -> "tile -boy -drown"
  BurnedBoy -> "tile -boy -burned"

tileToElem :: forall p i. DisplayTile -> HH.HTML p i
tileToElem tile = HH.span [ HP.class_ (H.ClassName (tileClasses tile)) ] []

-- | Builds a row of HTML tags including the parent one
-- | for given `DisplayTile`s
tilesRowElem :: forall p i. Array DisplayTile -> HH.HTML p i
tilesRowElem tiles =
  HH.div
    [ HP.class_ (H.ClassName "level-row") ]
    (map tileToElem tiles)

buildTile :: Point -> Game -> DisplayTile
buildTile p { state, level: { player, enemies, tiles, blocks } } = case tile of
  _ | p == player.pos -> case state of
      Dead Drown -> DrownBoy
      Dead Burned -> BurnedBoy
      _ -> case tile of
          Just Water -> Swimming player.direction
          _ -> Boy player.direction
  _ | Set.member p blocks -> Block
  Just (CloneMachine e) -> Tile (CloneMachine e)
  _ -> maybe (maybe Floor Tile tile) Creature (lookup p enemies)

  where

  tile = lookup p tiles
