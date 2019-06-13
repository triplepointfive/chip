module Chip.Mutation.Game
  ( onLevel
  ) where

import Chip.Model (Level, Game)

onLevel :: (Level -> Level) -> Game -> Game
onLevel f game = game { level = f game.level }
