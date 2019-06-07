module Chip.Game
  ( Game(..)
  , State(..)
  , onLevel
  ) where

import Prelude

import Data.Maybe

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

type Game =
  { level :: Level
  , levelNum :: Int
  , state :: State
  , name :: String
  , intactLevel :: Level
  , moving :: Maybe Direction
  }

onLevel :: (Level -> Level) -> Game -> Game
onLevel f game = game { level = f game.level }
