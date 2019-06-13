module Chip.Model.State
  ( State(..)
  ) where

import Prelude

import Chip.Model.DieReason (DieReason)

data State
  = Init
  | Alive
  | Dead DieReason
  | Pause
  | Complete

derive instance eqState :: Eq State
