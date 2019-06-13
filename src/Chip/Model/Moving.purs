module Chip.Model.Moving
  ( Moving(..)
  ) where

import Chip.Model.Direction (Direction)

data Moving
  = Pressed Direction
  | Released Direction
  | Processed Direction
  | Unpressed
