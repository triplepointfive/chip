module Game
  ( Game(..)
  , onLevel
  ) where

import Level (Level)

type Game =
  { level :: Level
  , levelNum :: Int
  }

onLevel :: (Level -> Level) -> Game -> Game
onLevel f game = game { level = f game.level }
