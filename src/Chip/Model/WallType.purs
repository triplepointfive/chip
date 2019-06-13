module Chip.Model.WallType
  ( WallType(..)
  ) where

import Prelude

import Chip.Model.Direction (Direction(..))

data WallType
  = Solid
  | Invisible
  | Hidden
  | Blue
  | Fake
  | Recessed
  | Flat Direction

derive instance eqWallType :: Eq WallType

instance showWallType :: Show WallType where
  show = case _ of
    Solid -> "#"
    Invisible -> "X"
    Hidden -> "H"
    Blue -> "%"
    Fake -> "'"
    Recessed -> "O"
    Flat dir -> "FLAT"
