module Chip.Tile
  ( Color(..)
  , Item(..)
  , Tile(..)
  ) where

import Prelude

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

-- | A single cell on a level grid
data Tile
  = Wall
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

derive instance eqTile :: Eq Tile

instance showTile :: Show Tile where
  show = case _ of
    Wall -> "#"
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
