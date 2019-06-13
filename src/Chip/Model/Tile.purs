module Chip.Model.Tile
  ( Tile(..)
  ) where

import Prelude

import Chip.Model.Color (Color(..))
import Chip.Model.Direction (Direction(..))
import Chip.Model.Enemy (Enemy)
import Chip.Model.Item (Item)
import Chip.Model.SwitchState (SwitchState(..))
import Chip.Model.WallType (WallType)

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
  | Gravel
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
  | Teleport

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
    Teleport -> "o"
    Gravel -> "░"
