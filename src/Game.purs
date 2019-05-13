module Game
  ( Game(..)
  ) where

import Level (Level)

data Game
  = Loading Int
  | Play Level
