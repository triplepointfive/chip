module Chip.Mutation.Level
  ( addEnemy
  , addBlock
  , act
  , moveBlock
  , moveToSocket
  , removeCurrentTile
  , onInventory
  , removeTile
  , toggleTanks
  , toggleWalls
  , pickUpChip
  , pickUp
  ) where

import Prelude

import Data.Array (uncons, filter, foldl, catMaybes)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set
import Data.Tuple (Tuple(..))

import Chip.Model (Direction(..), Enemy(..), Inventory, Item, Level, Point, Tile(..), isActiveTrap, isFloor)
import Chip.Mutation.Direction (invert, toLeft, toRight)
import Chip.Mutation.Enemy (toggleTank)
import Chip.Mutation.Inventory (addItem)
import Chip.Mutation.Point (adjustPoint)
import Chip.Mutation.Tile (toggleWall)

type ActResult =
  { level :: Level
  , old :: Map.Map Point Enemy
  }

moveBlock :: Point -> Point -> Level -> Level
moveBlock from to level = level
  { blocks = Set.insert to (Set.delete from level.blocks)
  }

moveToSocket :: Point -> Level -> Level
moveToSocket pos level
  | level.chipsLeft == 0 = removeCurrentTile level { player { pos = pos } }
  | otherwise            = level

removeCurrentTile :: Level -> Level
removeCurrentTile level = removeTile level.player.pos level

onInventory :: (Inventory -> Inventory) -> Level -> Level
onInventory f level = level { inventory = f level.inventory }

removeTile :: Point -> Level -> Level
removeTile point level = level { tiles = Map.delete point level.tiles }

addEnemy :: Point -> Enemy -> Level -> Level
addEnemy p enemy l = l { enemies = Map.insert p enemy l.enemies }

removeEnemy :: Point -> Level -> Level
removeEnemy p l = l { enemies = Map.delete p l.enemies }

addBlock :: Point -> Level -> Level
addBlock p l = l { blocks = Set.insert p l.blocks }

toggleTanks :: Level -> Level
toggleTanks level = level { enemies = map toggleTank level.enemies }

toggleWalls :: Level -> Level
toggleWalls level = level { tiles = map toggleWall level.tiles }

pickUpChip :: Level -> Level
pickUpChip level
  | level.chipsLeft == 0 = removeCurrentTile level
  | otherwise = removeCurrentTile level { chipsLeft = level.chipsLeft - 1 }

pickUp :: Item -> Level -> Level
pickUp item = removeCurrentTile <<< onInventory (addItem item)

act :: Level -> Level
act initLevel = foldEnemies
  { level: initLevel
  , old: initLevel.enemies
  }

  where

  add :: Point -> Enemy -> ActResult -> ActResult
  add pos enemy { level, old } =
    { level: addEnemy pos enemy level
    , old
    }

  -- TODO: Check for water & fire
  -- TODO: Check per monster type
  withNewPos :: Point -> Point -> Enemy -> Level -> Level
  withNewPos pos oldPos enemy level = case Map.lookup pos level.tiles of
    Just CloneMachineButton -> cloneEnemies (removeEnemy oldPos $ addEnemy pos enemy level)
    Just Water -> removeEnemy oldPos level
    Just Bomb -> removeEnemy oldPos $ removeTile pos level
    _ -> removeEnemy oldPos $ addEnemy pos enemy level

  foldEnemies :: ActResult -> Level
  foldEnemies { level, old } = case Map.findMin old of
    Just { key: oldPos, value: oldEnemy } ->
        let { pos, enemy } = act' level oldPos oldEnemy in
        if pos == oldPos
            then foldEnemies { level, old: Map.delete oldPos old }
            else foldEnemies { level: withNewPos pos oldPos enemy level, old: Map.delete oldPos old }
    Nothing -> level

  goTo :: Level -> Point -> Direction -> (Direction -> Enemy) -> Array Direction -> { pos :: Point, enemy :: Enemy }
  goTo level pos direction enemy dirs =
    case uncons (filter (isFloor level <<< adjustPoint pos) dirs) of
        Just { head: floorDirection } ->
            { pos: adjustPoint pos floorDirection
            , enemy: enemy floorDirection
            }
        Nothing -> { pos, enemy: enemy direction }

  act' :: Level -> Point -> Enemy -> { pos :: Point, enemy :: Enemy }
  act' level pos@({ x, y }) = case _ of
    enemy | isActiveTrap pos level -> { pos, enemy }

    Bee direction -> goTo level pos direction Bee
        [toLeft direction, direction, toRight direction, invert direction]

    FireBall direction -> goTo level pos direction FireBall
        [direction, toRight direction, toLeft direction, invert direction]

    Glider direction -> goTo level pos direction Glider
        [direction, toLeft direction, toRight direction, invert direction]

    Tank direction | isFloor level (adjustPoint pos direction) ->
        { pos: adjustPoint pos direction, enemy: Tank direction }
    Tank direction -> { pos, enemy: Tank direction }

    Ball direction | isFloor level (adjustPoint pos direction) ->
        { pos: adjustPoint pos direction, enemy: Ball direction }
    Ball direction | isFloor level (adjustPoint pos (invert direction)) ->
        { pos: adjustPoint pos (invert direction), enemy: Ball (invert direction) }
    Ball direction ->
        { pos, enemy: Ball (invert direction) }

    Teeth direction | mod level.tick 4 /= 0 -> { pos, enemy: Teeth direction }
    Teeth direction -> goTo level pos direction Teeth $
        catMaybes [verticalDirection, horizontalDirection]

    where

    verticalDirection
      | y > level.player.pos.y = Just Up
      | y < level.player.pos.y = Just Down
      | otherwise = Nothing

    horizontalDirection
      | x > level.player.pos.x = Just Left
      | x < level.player.pos.x = Just Right
      | otherwise = Nothing

cloneEnemies :: Level -> Level
cloneEnemies level =
  foldl withTile level (Map.toUnfoldable level.tiles :: Array (Tuple Point Tile))

  where

  withTile lvl (Tuple pos tile) = case tile of
    CloneMachine newEnemy -> addEnemy pos newEnemy lvl
    _ -> lvl
