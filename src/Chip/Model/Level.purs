module Chip.Model.Level
  ( Level
  , Player
  , Tiles
  , isActiveTrap
  , visibleHint
  , mapSize
  , outOfLevel
  , isFloor
  , hasEnemy
  , enemyAt
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set

import Chip.Model.Enemy (Enemy)
import Chip.Model.Direction (Direction)
import Chip.Model.Inventory (Inventory)
import Chip.Model.Point (Point)
import Chip.Model.Tile (Tile(..))
import Chip.Model.SwitchState (SwitchState(..))

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

-- TODO: Check for blocks as well
isActiveTrap :: Point -> Level -> Boolean
isActiveTrap point level = case Map.lookup point level.trapConnections of
  _ | Map.lookup point level.tiles /= Just Trap -> false
  Just button | button == level.player.pos -> false
  Just button | Set.member button level.blocks -> false
  _ -> true

-- | Returns hint text if it should be shown
visibleHint :: Level -> Maybe String
visibleHint { tiles, player: { pos }, hint } = case Map.lookup pos tiles of
  Just Hint -> hint
  _ -> Nothing

-- | Height and width of a level grid
mapSize :: Int
mapSize = 32 -- TODO: Move into a level

outOfLevel :: Point -> Boolean
outOfLevel { x, y } = x < 0 || y < 0 || x >= mapSize || y >= mapSize

-- TODO: Check for blocks
isFloor :: Level -> Point -> Boolean
isFloor level pos = case Map.lookup pos level.tiles of
  _ | Set.member pos level.blocks -> false
  _ | hasEnemy pos level -> false
  Nothing -> true
  Just CloneMachineButton -> true
  Just Fire -> true
  Just Water -> true
  Just (SwitchableWall Off) -> true
  Just Bomb -> true
  Just Trap -> true
  Just Ice -> true
  _ -> false

hasEnemy :: Point -> Level -> Boolean
hasEnemy point { enemies } = Map.member point enemies

enemyAt :: Point -> Level -> Maybe Enemy
enemyAt point { enemies } = Map.lookup point enemies
