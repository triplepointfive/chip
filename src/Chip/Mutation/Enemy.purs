module Chip.Mutation.Enemy
  ( toggleTank
  ) where

import Chip.Model (Enemy(..))
import Chip.Mutation.Direction (invert)

toggleTank :: Enemy -> Enemy
toggleTank = case _ of
  Tank direction -> Tank (invert direction)
  e -> e
