module Chip.Model.Enemy
  ( Enemy(..)
  ) where

import Prelude

import Chip.Model.Direction (Direction)

-- | Living creature to avoid
data Enemy
  = Bee Direction
  | Tank Direction
  | Ball Direction
  | FireBall Direction
  | Glider Direction
  | Teeth Direction

derive instance eqEnemy :: Eq Enemy

instance showEnemy :: Show Enemy where
  show = case _ of
    Bee direction -> "Bee " <> show direction
    Tank direction -> "Tank " <> show direction
    Ball direction -> "#Ball " <> show direction
    FireBall direction -> "FireBall " <> show direction
    Glider direction -> "Glider " <> show direction
    Teeth direction -> "Teeth " <> show direction
