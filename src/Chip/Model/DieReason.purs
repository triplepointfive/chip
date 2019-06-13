module Chip.Model.DieReason
  ( DieReason(..)
  ) where

import Prelude

data DieReason
  = Drown
  | Burned
  | Eaten
  | Timed
  | BlownUp

derive instance eqDieReason :: Eq DieReason

instance showDieReason :: Show DieReason where
  show = case _ of
    Drown -> "Drown"
    Burned -> "Burned"
    Eaten -> "Eaten"
    Timed -> "Timed"
    BlownUp -> "BlownUp"
