module Game
  ( Game(..)
  , State(..)
  , onLevel
  , tick
  ) where

import Prelude

import Level (Level)

data State
  = Alive
  | Dead String
  | Pause
  | Complete

type Game =
  { level :: Level
  , levelNum :: Int
  , ticksLeft :: Int
  , state :: State
  }

tick :: Game -> Game
tick game = game { ticksLeft = game.ticksLeft - 1 }

onLevel :: (Level -> Level) -> Game -> Game
onLevel f game = game { level = f game.level }
