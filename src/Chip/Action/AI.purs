module Chip.Action.AI
  ( actAI
  ) where

import Prelude

import Chip.Action (ActionResult, inactive)
import Chip.Model (Level)
import Chip.Mutation (act)

actAI :: Level -> ActionResult Level
actAI = inactive <<< act
