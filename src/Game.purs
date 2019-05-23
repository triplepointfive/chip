module Game
  ( Game(..)
  , State(..)
  , onLevel
  , tick
  ) where

import Prelude

import Chip.Action (DieReason)
import Level (Level)

data State
  = Init
  | Alive
  | Dead DieReason
  | Pause
  | Complete

derive instance eqState :: Eq State

type Game =
  { level :: Level
  , levelNum :: Int
  , ticksLeft :: Int
  , state :: State
  , name :: String
  }

tick :: Game -> Game
tick game = game { ticksLeft = game.ticksLeft - 1 }

onLevel :: (Level -> Level) -> Game -> Game
onLevel f game = game { level = f game.level }
