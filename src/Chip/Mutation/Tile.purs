module Chip.Mutation.Tile
  ( toggleWall
  ) where

import Chip.Model (Tile(..), SwitchState(..))

toggleWall :: Tile -> Tile
toggleWall = case _ of
  SwitchableWall On -> SwitchableWall Off
  SwitchableWall Off -> SwitchableWall On
  t -> t
