module Chip.Enemy
  ( Enemy(..)
  ) where

import Prelude

import Utils (Direction)

-- | Living creature to avoid
data Enemy
  = Bee Direction
  | Tank Direction
  | Ball Direction
  | FireBall Direction

derive instance eqEnemy :: Eq Enemy

instance showEnemy :: Show Enemy where
  show = case _ of
    Bee direction -> "Bee " <> show direction
    Tank direction -> "Tank " <> show direction
    Ball direction -> "#Ball " <> show direction
    FireBall direction -> "FireBall " <> show direction
