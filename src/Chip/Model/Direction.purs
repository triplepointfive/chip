module Chip.Model.Direction
  ( Direction(..)
  ) where

import Prelude

-- | 4 side direction
data Direction
  = Up
  | Down
  | Left
  | Right

derive instance eqDirection :: Eq Direction

instance showDirection :: Show Direction where
  show = case _ of
    Up -> "Up"
    Down -> "Down"
    Left -> "Left"
    Right -> "Right"
