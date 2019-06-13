module Chip.Model.Level
  ( Level
  , Player
  , Tiles
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe)
import Data.Set as Set

import Chip.Model.Enemy (Enemy)
import Chip.Model.Direction (Direction)
import Chip.Model.Inventory (Inventory)
import Chip.Model.Point (Point)
import Chip.Model.Tile (Tile)

-- | Mapping for cell coordinates to object on it.
-- | Does not include floor for simplicity
type Tiles = Map.Map Point Tile

-- | Main character and its data
type Player =
  { pos :: Point
  , direction :: Direction
  , turnedAt :: Int
  , movedAt :: Int
  }

-- | Represents a floor with all its objects
type Level =
  { player :: Player
  , tiles :: Tiles
  , inventory :: Inventory
  , chipsLeft :: Int
  , hint :: Maybe String
  , enemies :: Map.Map Point Enemy
  , blocks :: Set.Set Point
  , trapConnections :: Map.Map Point Point
  , ticksLeft :: Maybe Int
  , tick :: Int
  }
