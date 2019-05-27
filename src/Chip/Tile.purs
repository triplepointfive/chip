module Chip.Tile
  ( Color(..)
  , Item(..)
  , Tile(..)
  , WallType(..)
  ) where

import Prelude

import Chip.Enemy (Enemy)
import Utils (Direction(..), SwitchState(..))

-- | Colors for keys and related doors
data Color
  = Red
  | Cyan
  | Yellow
  | Green

derive instance eqColor :: Eq Color

-- | What can be found on floor and picked up
data Item
  = SkiSkates
  | SuctionBoots
  | FireBoots
  | Flippers
  | Key Color

derive instance eqItem :: Eq Item

instance showItem :: Show Item where
  show = case _ of
    SkiSkates -> "S"
    SuctionBoots -> "U"
    FireBoots -> "I"
    Flippers -> "F"
    Key color -> case color of
      Red -> "r"
      Cyan -> "c"
      Yellow -> "y"
      Green -> "g"

data WallType
  = Solid
  | Invisible
  | Hidden
  | Blue
  | Fake
  | Recessed
  | Flat Direction

derive instance eqWallType :: Eq WallType

instance showWallType :: Show WallType where
  show = case _ of
    Solid -> "#"
    Invisible -> "X"
    Hidden -> "H"
    Blue -> "%"
    Fake -> "'"
    Recessed -> "O"
    Flat dir -> "FLAT"

-- | A single cell on a level grid
data Tile
  = Wall WallType
  | Door Color
  | Chip
  | Socket
  | Exit
  | Hint
  | Water
  | Dirt
  | Fire
  | Item Item
  | Force Direction
  | Ice
  | IceCorner Direction
  | SwitchableWall SwitchState
  | WallButton
  | TankButton
  | Bomb
  | CloneMachine Enemy
  | CloneMachineButton
  | Trap
  | TrapButton
  | Thief

derive instance eqTile :: Eq Tile

instance showTile :: Show Tile where
  show = case _ of
    Wall t -> show t
    Door color -> case color of
      Red -> "R"
      Cyan -> "C"
      Yellow -> "Y"
      Green -> "G"
    Chip -> "+"
    Socket -> "-"
    Exit -> "<"
    Hint -> "?"
    Water -> "~"
    Dirt -> "≈"
    Fire -> "^"
    Item item -> show item
    Force direction -> case direction of
      Down -> "↓"
      Left -> "←"
      Up -> "↑"
      Right -> "→"
    Ice -> "╬"
    IceCorner direction -> case direction of
      Down -> "╝"
      Left -> "╚"
      Up -> "╔"
      Right -> "╗"
    SwitchableWall On -> "["
    SwitchableWall Off -> "]"
    WallButton -> "."
    TankButton -> ","
    Bomb -> "*"
    CloneMachine enemy -> "<-." <> show enemy
    CloneMachineButton -> "↺"
    Trap -> ";"
    TrapButton -> ":"
    Thief -> "t"
