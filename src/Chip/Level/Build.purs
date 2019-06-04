module Chip.Level.Build
  ( Blank(..)
  , Connection(..)
  , build
  ) where

import Prelude

import Data.Array (foldl, foldr)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Set as Set
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))

import Chip.Enemy (Enemy(..))
import Chip.Inventory (initInventory)
import Chip.Tile (Tile(..), Item(..), Color(..), WallType(..))
import Level (Level, addEnemy)
import Utils (Direction(..), Point, SwitchState(..), addIndex)

type Connection =
  { button :: Point
  , trap :: Point
  }

-- | Structure used to build a level
type Blank =
  { grid :: Array String
  , hint :: Maybe String
  , name :: String
  , chipsLeft :: Int
  , timeLimit :: Int
  , blocks :: Array Point
  , trapConnections :: Array Connection
  , extraChips :: Array Point
  }

-- | Builds a level from its blank
build :: Blank -> Level
build { grid, hint, chipsLeft, blocks, trapConnections, extraChips } =
  foldl
    (\level { i: y, v: row } ->
      foldr
        (\{ i: x, v: c } -> addCell { x: x, y: y } c)
        level
        row
    )
    initLevel
    (addIndex $ map (addIndex <<< toCharArray) grid)

  where

  initLevel :: Level
  initLevel =
    { player: { pos: { x: 0, y: 0 }, direction: Down }
    , tiles: Map.empty
    , inventory: initInventory
    , chipsLeft
    , enemies: Map.empty
    , trapConnections: buildConnections trapConnections
    , blocks: Set.fromFoldable blocks
    , hint
    , chips: Set.fromFoldable extraChips
    }

  addCell :: Point -> Char -> Level -> Level
  addCell p c = case c of
    ' ' -> identity

    '#' -> insertTile (Wall Solid)
    'X' -> insertTile (Wall Invisible)
    'H' -> insertTile (Wall Hidden)
    '%' -> insertTile (Wall Blue)
    '\'' -> insertTile (Wall Fake)
    'O' -> insertTile (Wall Recessed)
    '_' -> insertTile (Wall (Flat Down))

    '+' -> addChip
    'r' -> insertTile (Item (Key Red))
    'c' -> insertTile (Item (Key Cyan))
    'y' -> insertTile (Item (Key Yellow))
    'g' -> insertTile (Item (Key Green))
    'R' -> insertTile (Door Red)
    'C' -> insertTile (Door Cyan)
    'Y' -> insertTile (Door Yellow)
    'G' -> insertTile (Door Green)
    'b' -> addEnemy p (Bee Up)
    '~' -> insertTile Water
    '^' -> insertTile Fire
    't' -> insertTile Thief

    '⊤' -> addEnemy p (Tank Down)
    '⊣' -> addEnemy p (Tank Left)
    '⊥' -> addEnemy p (Tank Up)
    '⊢' -> addEnemy p (Tank Right)

    '↓' -> insertTile (Force Down)
    '←' -> insertTile (Force Left)
    '↑' -> insertTile (Force Up)
    '→' -> insertTile (Force Right)

    '*' -> insertTile Bomb

    '@' -> _ { player { pos = p } }
    '-' -> insertTile Socket
    '<' -> insertTile Exit
    '?' -> insertTile Hint
    'o' -> insertTile Teleport

    '‖' -> addEnemy p (Ball Up)
    '=' -> addEnemy p (Ball Left)

    '⇠' -> insertTile (CloneMachine (FireBall Left))
    '⇡' -> insertTile (CloneMachine (FireBall Up))
    '⇢' -> insertTile (CloneMachine (FireBall Right))
    '⇣' -> insertTile (CloneMachine (FireBall Down))

    '☝' -> addEnemy p (Glider Up)
    '☞' -> addEnemy p (Glider Right)
    '☟' -> addEnemy p (Glider Down)
    '☜' -> addEnemy p (Glider Left)

    'S' -> insertTile (Item SkiSkates)
    'U' -> insertTile (Item SuctionBoots)
    'I' -> insertTile (Item FireBoots)
    'F' -> insertTile (Item Flippers)

    '╝' -> insertTile (IceCorner Down)
    '╚' -> insertTile (IceCorner Left)
    '╔' -> insertTile (IceCorner Up)
    '╗' -> insertTile (IceCorner Right)
    '╬' -> insertTile Ice

    '[' -> insertTile (SwitchableWall On)
    ']' -> insertTile (SwitchableWall Off)

    '.' -> insertTile WallButton
    ',' -> insertTile TankButton
    ':' -> insertTile TrapButton
    '↺' -> insertTile CloneMachineButton
    ';' -> insertTile Trap

    '≈' -> insertTile Dirt
    _   -> identity

    where

    insertTile :: Tile -> Level -> Level
    insertTile tile l = l { tiles = Map.insert p tile l.tiles }

    addChip :: Level -> Level
    addChip l = l { chips = Set.insert p l.chips }

buildConnections :: Array Connection -> Map.Map Point Point
buildConnections = Map.fromFoldable <<< map (\ { trap, button } -> Tuple trap button)
