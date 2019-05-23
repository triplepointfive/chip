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

import Chip.Enemy (Enemy(..))
import Chip.Tile (Color(..), Tile(..), Item(..))
import Level (mapSize)
import Level as Level
import Game (Game, State(..))
import Utils (Direction(..), Point, SwitchState(..))

-- | What shall be output on screen
data DisplayTile
  = Floor
  | Tile Tile
  | Boy Direction
  | Creature Enemy
  | Swimming Direction
  | Drown
  | Burned
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
  Tile Exit -> "tile -exit"
  Tile Dirt -> "tile -dirt"
  Tile Hint -> "tile -hint"
  Tile Socket -> "tile -socket"
  Drown -> "tile -boy -drown"
  Burned -> "tile -boy -burned"

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
buildTile p game = withTile (lookup p game.level.tiles)
  where
  { player: { pos, direction } } = game.level

  withTile tile
    | p == pos = case game.state of
      Dead Level.Drown -> Drown
      Dead Level.Burned -> Burned
      _ -> boyTile tile
    | Set.member p game.level.blocks = Block
    | otherwise = maybe (maybe Floor Tile tile) Creature (lookup p game.level.enemies)

  boyTile tile
    | tile == Just Water = Swimming direction
    | otherwise = Boy direction
