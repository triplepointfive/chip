module Chip.Action.Tick
  ( tick
  ) where

import Prelude

import Chip.Action (Action(..), ActionResult, DieReason(..), inactive, withAction)
import Level (Level)
import Chip.Utils (Direction(..))

tick :: Level -> ActionResult Level
tick level = case unit of
  _ | level.ticksLeft == 1 ->
      withAction ticked (Die Timed)
  _ | level.ticksLeft < level.player.turnedAt - 4 ->
      inactive $ ticked { player { direction = Down } }
  _ -> inactive ticked

  where

  ticked = level { ticksLeft = level.ticksLeft - 1 }
