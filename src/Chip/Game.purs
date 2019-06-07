module Chip.Game
  ( Game(..)
  , State(..)
  , Moving(..)
  , onLevel
  ) where

import Prelude

import Chip.Action (DieReason)
import Level (Level)
import Chip.Utils (Direction)

data State
  = Init
  | Alive
  | Dead DieReason
  | Pause
  | Complete

derive instance eqState :: Eq State

data Moving
  = Pressed Direction
  | Released Direction
  | Processed Direction
  | Unpressed

type Game =
  { level :: Level
  , levelNum :: Int
  , state :: State
  , name :: String
  , intactLevel :: Level
  , moving :: Moving
  }

onLevel :: (Level -> Level) -> Game -> Game
onLevel f game = game { level = f game.level }
