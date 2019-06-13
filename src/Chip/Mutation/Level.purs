module Chip.Mutation.Level
  ( addEnemy
  , addBlock
  , moveBlock
  , moveToSocket
  , countChip
  , removeCurrentTile
  , onInventory
  , removeTile
  , toggleTanks
  , toggleWalls
  ) where

import Prelude

import Data.Map as Map
import Data.Set as Set

import Chip.Model (Level, Inventory, Point, Enemy)
import Chip.Mutation.Enemy (toggleTank)
import Chip.Mutation.Tile (toggleWall)

moveBlock :: Point -> Point -> Level -> Level
moveBlock from to level = level
  { blocks = Set.insert to (Set.delete from level.blocks)
  }

moveToSocket :: Point -> Level -> Level
moveToSocket pos level
  | level.chipsLeft == 0 = removeCurrentTile level { player { pos = pos } }
  | otherwise            = level

countChip :: Level -> Level
countChip level
  | level.chipsLeft == 0 = level
  | otherwise            = level { chipsLeft = level.chipsLeft - 1 }

removeCurrentTile :: Level -> Level
removeCurrentTile level = removeTile level.player.pos level

onInventory :: (Inventory -> Inventory) -> Level -> Level
onInventory f level = level { inventory = f level.inventory }

removeTile :: Point -> Level -> Level
removeTile point level = level { tiles = Map.delete point level.tiles }

addEnemy :: Point -> Enemy -> Level -> Level
addEnemy p enemy l = l { enemies = Map.insert p enemy l.enemies }

addBlock :: Point -> Level -> Level
addBlock p l = l { blocks = Set.insert p l.blocks }

toggleTanks :: Level -> Level
toggleTanks level = level { enemies = map toggleTank level.enemies }

toggleWalls :: Level -> Level
toggleWalls level = level { tiles = map toggleWall level.tiles }
