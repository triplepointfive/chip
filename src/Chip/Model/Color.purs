module Chip.Model.Color
  ( Color(..)
  ) where

import Prelude

-- | Colors for keys and related doors
data Color
  = Red
  | Cyan
  | Yellow
  | Green

derive instance eqColor :: Eq Color
