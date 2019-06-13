module Chip.Mutation.Point
  ( adjustPoint
  ) where

import Prelude

import Chip.Model (Direction(..), Point)

adjustPoint :: Point -> Direction -> Point
adjustPoint { x, y } direction = case direction of
  Up -> { x, y: y - 1 }
  Left -> { x: x - 1, y }
  Down -> { x, y: y + 1 }
  Right -> { x: x + 1, y }

