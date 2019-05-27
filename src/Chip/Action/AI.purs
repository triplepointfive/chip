module Chip.Action.AI
  ( actAI
  ) where

import Prelude

import Data.Array (uncons, filter, foldl)
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import Chip.Action (ActionResult, inactive)
import Chip.Enemy (Enemy(..))
import Chip.Tile (Tile(..))
import Level (Level, addEnemy, removeTile)
import Utils (Direction, Point, SwitchState(..), adjustPoint, toLeft, toRight, invert)

type ActResult =
  { level :: Level
  , old :: Map.Map Point Enemy
  }

actAI :: Level -> ActionResult Level
actAI initLevel = inactive $ foldEnemies
  { level: initLevel { enemies = Map.empty }
  , old: initLevel.enemies
  }

  where

  add :: Point -> Enemy -> ActResult -> ActResult
  add pos enemy { level, old } =
    { level: level { enemies = Map.insert pos enemy level.enemies }
    , old
    }

  cloneEnemies :: Level -> Level
  cloneEnemies level =
    foldl withTile level (Map.toUnfoldable level.tiles :: Array (Tuple Point Tile))

    where

    withTile lvl (Tuple pos tile) = case tile of
      CloneMachine newEnemy -> addEnemy pos newEnemy lvl
      _ -> lvl

  -- TODO: Check for water & fire
  -- TODO: Check per monster type
  -- TODO: Remove bomb if blown up
  withNewPos :: Point -> Enemy -> Level -> Level
  withNewPos pos enemy level = case Map.lookup pos level.tiles of
    Just CloneMachineButton -> cloneEnemies (addEnemy pos enemy level)
    Just Water -> level
    Just Bomb -> removeTile pos level
    _ -> addEnemy pos enemy level

  foldEnemies :: ActResult -> Level
  foldEnemies { level, old } = case Map.findMin old of
    Just { key: oldPos, value: oldEnemy } ->
        let { pos, enemy } = act level oldPos oldEnemy in
        if pos == oldPos
            then foldEnemies $ add pos enemy { level, old: Map.delete oldPos old }
            else foldEnemies { level: withNewPos pos enemy level, old: Map.delete oldPos old }
    Nothing -> level

  -- TODO: Check for blocks
  isFloor :: Level -> Point -> Boolean
  isFloor level p = case Map.lookup p level.tiles of
    Nothing -> true
    Just CloneMachineButton -> true
    Just Fire -> true
    Just Water -> true
    Just (SwitchableWall Off) -> true
    Just Bomb -> true
    Just (Trap _) -> true
    _ -> false

  goTo :: Level -> Point -> Direction -> (Direction -> Enemy) -> Array Direction -> { pos :: Point, enemy :: Enemy }
  goTo level pos direction enemy dirs =
    case uncons (filter (isFloor level <<< adjustPoint pos) dirs) of
        Just { head: floorDirection } ->
            { pos: adjustPoint pos floorDirection
            , enemy: enemy floorDirection
            }
        Nothing -> { pos, enemy: enemy direction }

  act :: Level -> Point -> Enemy -> { pos :: Point, enemy :: Enemy }
  act level pos (Bee direction) = goTo level pos direction Bee
      [toLeft direction, direction, toRight direction, invert direction]
  act level pos (FireBall direction) = goTo level pos direction FireBall
      [direction, toRight direction, toLeft direction, invert direction]
  act level pos (Glider direction) = goTo level pos direction Glider
      [direction, toLeft direction, toRight direction, invert direction]
  act level pos (Tank direction)
    | isFloor level (adjustPoint pos direction) = { pos: adjustPoint pos direction, enemy: Tank direction }
    | otherwise = { pos, enemy: Tank direction }
  act level pos (Ball direction) = case unit of
    _ | isFloor level dest -> { pos: dest, enemy: Ball direction }
    _ | isFloor level invertDest -> { pos: invertDest, enemy: Ball invertDir }
    _ -> { pos, enemy: Ball (invert direction) }

    where

    invertDir = invert direction
    invertDest = adjustPoint pos invertDir
    dest = adjustPoint pos direction
