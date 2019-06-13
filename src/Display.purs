module Display
  ( DisplayTile(..)
  , levelTiles
  , tilesRowElem
  ) where

import Prelude

import Data.Array (range)
import Data.Map (lookup)
import Data.Maybe (maybe)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Chip.Model (Game, State(..), DieReason(..), Point, Enemy(..), Color(..), Tile(..), Item(..), WallType(..), Direction(..), SwitchState(..), mapSize, enemyAt)

-- | What shall be output on screen
data DisplayTile
  = Floor
  | Tile Tile
  | Boy Direction DisplayTile
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
  map (flip Tuple xRange) (range (from y) (to y))

  where

  xRange = range (from x) (to x)
  d = (2 * r) + 1
  to c = min (mapSize - 1) (d + from c - 1)
  from c = max 0 (min (max 0 (c - r)) (mapSize - d))

tileClasses :: DisplayTile -> String
tileClasses = case _ of
  Floor -> "tile -floor"

  Tile (Wall Solid) -> "tile -wall"
  Tile (Wall Invisible) -> "tile -floor"
  Tile (Wall Hidden) -> "tile -floor"
  Tile (Wall Blue) -> "tile -wall -blue"
  Tile (Wall Fake) -> "tile -wall -blue"
  Tile (Wall Recessed) -> "tile -wall -recessed"
  Tile (Wall (Flat direction)) -> "tile -wall" <> directionSuffix direction
  Tile Thief -> "tile -thief"

  Tile Teleport -> "tile -teleport"
  Tile Chip -> "tile -chip"
  Tile (Door Red) -> "tile -door -red"
  Tile (Door Cyan) -> "tile -door -cyan"
  Tile (Door Yellow) -> "tile -door -yellow"
  Tile (Door Green) -> "tile -door -green"
  Tile (Force direction) -> "tile -force" <> directionSuffix direction

  Boy direction tile -> tileClasses tile <> " -boy" <> directionSuffix direction <> "-creature"
  Creature (Bee direction) -> "tile -bee" <> directionSuffix direction
  Creature (Teeth direction) -> "tile -teeth" <> directionSuffix direction
  Creature (Glider direction) -> "tile -glider" <> directionSuffix direction
  Creature (Tank direction) -> "tile -tank" <> directionSuffix direction
  Creature (Ball _) -> "tile -ball"
  Creature (FireBall _) -> "tile -fireball"

  Tile (CloneMachine _) -> "tile -clone-machine"
  Block -> "tile -block"
  Swimming direction -> "tile -swimming" <> directionSuffix direction <> "-creature"
  Tile Bomb -> "tile -bomb"
  Tile Water -> "tile -water"
  Tile Fire -> "tile -fire"
  Tile Ice -> "tile -ice"
  Tile (IceCorner direction) -> "tile -ice -corner" <> directionSuffix direction
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
  Tile Gravel -> "tile -gravel"
  Tile Hint -> "tile -hint"
  Tile Socket -> "tile -socket"
  Tile Trap -> "tile -trap"
  Tile TrapButton -> "tile -trap-button"
  DrownBoy -> "tile -drown"
  BurnedBoy -> "tile -burned"

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
buildTile p { state, level: level@{ player, tiles, blocks } } = case tile of
  _ | p == player.pos -> case state of
      Dead Drown -> DrownBoy
      Dead Burned -> BurnedBoy
      _ -> case tile of
          Tile Water -> Swimming player.direction
          _ -> Boy player.direction tile
  _ | Set.member p blocks -> Block
  -- Just (CloneMachine e) -> Tile (CloneMachine e)
  _ -> maybe tile Creature (enemyAt p level)

  where

  tile = maybe Floor Tile (lookup p tiles)

directionSuffix :: Direction -> String
directionSuffix = case _ of
  Left -> " -left"
  Up -> " -up"
  Right -> " -right"
  Down -> " -down"
