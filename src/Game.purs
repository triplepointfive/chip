module Game
  ( Game(..)
  , onLevel
  , tick
  ) where

import Prelude

import Level (Level)

type Game =
  { level :: Level
  , levelNum :: Int
  , ticksLeft :: Int
  }

tick :: Game -> Game
tick game = game { ticksLeft = game.ticksLeft - 1 }

onLevel :: (Level -> Level) -> Game -> Game
onLevel f game = game { level = f game.level }
