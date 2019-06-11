module Chip.Action.Tick
  ( tick
  ) where

import Prelude

import Data.Maybe (Maybe(..))

import Chip.Action (Action(..), ActionResult, DieReason(..), inactive, withAction)
import Level (Level)
import Chip.Utils (Direction(..))

tick :: Level -> ActionResult Level
tick level = case unit of
  _ | level.ticksLeft == Just 1 ->
      withAction ticked (Die Timed)
  _ | level.tick - level.player.turnedAt > 4 ->
      inactive $ ticked { player { direction = Down } }
  _ -> inactive ticked

  where

  ticked = level
      { ticksLeft = (_ - 1) <$> level.ticksLeft
      , tick = level.tick + 1
      }
