module Chip.Model.Game
  ( Game
  , notDead
  ) where

import Chip.Model.Level (Level)
import Chip.Model.Moving (Moving)
import Chip.Model.State (State(..))

type Game =
  { level :: Level
  , levelNum :: Int
  , state :: State
  , name :: String
  , intactLevel :: Level
  , moving :: Moving
  }

notDead :: Game -> Boolean
notDead game = case game.state of
  Dead _ -> false
  _ -> true
