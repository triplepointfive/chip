module Chip.Level.Build
  ( Blank(..)
  , build
  ) where

import Prelude

import Data.Array (foldl, foldr)
import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Set as Set
import Data.String.CodeUnits (toCharArray)

import Chip.Enemy (Enemy(..))
import Chip.Inventory (initInventory)
import Chip.Tile (Tile(..), Item(..), Color(..))
import Level (Level)
import Utils (Direction(..), Point, SwitchState(..), addIndex)

-- | Structure used to build a level
type Blank =
  { grid :: Array String
  , hint :: Maybe String
  , name :: String
  , chips :: Int
  , timeLimit :: Int
  , blocks :: Array Point
  }

-- | Builds a level from its blank
build :: Blank -> Level
build { grid, hint, chips, blocks } =
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
    , chipsLeft: chips
    , enemies: Map.empty
    , blocks: Set.fromFoldable blocks
    , hint
    }

  addCell :: Point -> Char -> Level -> Level
  addCell p c = case c of
    ' ' -> identity
    '#' -> insertTile Wall
    '+' -> insertTile Chip
    'r' -> insertTile (Item (Key Red))
    'c' -> insertTile (Item (Key Cyan))
    'y' -> insertTile (Item (Key Yellow))
    'g' -> insertTile (Item (Key Green))
    'R' -> insertTile (Door Red)
    'C' -> insertTile (Door Cyan)
    'Y' -> insertTile (Door Yellow)
    'G' -> insertTile (Door Green)
    'b' -> addEnemy (Bee Up)
    '~' -> insertTile Water
    '^' -> insertTile Fire

    '⊤' -> addEnemy (Tank Down)
    '⊣' -> addEnemy (Tank Left)
    '⊥' -> addEnemy (Tank Up)
    '⊢' -> addEnemy (Tank Right)

    '↓' -> insertTile (Force Down)
    '←' -> insertTile (Force Left)
    '↑' -> insertTile (Force Up)
    '→' -> insertTile (Force Right)

    '*' -> insertTile Bomb

    '@' -> _ { player { pos = p } }
    '-' -> insertTile Socket
    '<' -> insertTile Exit
    '?' -> insertTile Hint

    '‖' -> addEnemy (Ball Up)
    '=' -> addEnemy (Ball Left)

    '⇠' -> insertTile (CloneMachine (FireBall Left))
    '⇡' -> insertTile (CloneMachine (FireBall Up))
    '⇢' -> insertTile (CloneMachine (FireBall Right))
    '⇣' -> insertTile (CloneMachine (FireBall Down))

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
    '↺' -> insertTile CloneMachineButton

    '≈' -> insertTile Dirt
    _   -> identity

    where

    addEnemy :: Enemy -> Level -> Level
    addEnemy enemy l = l { enemies = Map.insert p enemy l.enemies }

    insertTile :: Tile -> Level -> Level
    insertTile tile l = l { tiles = Map.insert p tile l.tiles}
