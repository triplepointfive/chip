module Chip.Enemy
  ( Enemy(..)
  ) where

import Utils (Direction)

-- | Living creature to avoid
data Enemy
  = Bee Direction
  | Tank Direction
  | Ball Direction
