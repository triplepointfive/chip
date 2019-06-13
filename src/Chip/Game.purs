module Chip.Game
  ( Game(..)
  , Moving(..)
  , notDead
  , onLevel
  ) where

import Chip.Model.Direction (Direction)
import Chip.Model.Level (Level)
import Chip.Model.State (State(..))

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

notDead :: Game -> Boolean
notDead game = case game.state of
  Dead _ -> false
  _ -> true
