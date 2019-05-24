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
import Level (Level)
import Utils (Direction, Point, SwitchState(..), adjustPoint, toLeft, toRight, invert)

actAI :: Level -> ActionResult Level
actAI level = inactive $ level { enemies = actedEnemies }
  where

  actedEnemies = foldEnemies { new: Map.empty, old: level.enemies }

  cloneEnemies new = foldl withTile new (Map.toUnfoldable level.tiles :: Array (Tuple Point Tile))
    where

    withTile xs (Tuple pos tile) = case tile of
      CloneMachine newEnemy -> Map.insert pos newEnemy xs
      _ -> xs

  -- TODO: Check for water & fire
  -- TODO: Check per monster type
  withNewPos pos enemy { new, old } = case Map.lookup pos level.tiles of
    Just CloneMachineButton -> { new: cloneEnemies (Map.insert pos enemy new), old }
    Just Water -> { new, old }
    _ -> { new: (Map.insert pos enemy new), old }

  foldEnemies { new, old } = case Map.findMin old of
    Just { key: oldPos, value: oldEnemy } ->
        let { pos, enemy } = act oldPos oldEnemy in
        if pos == oldPos
            then foldEnemies { new: Map.insert pos enemy new, old: Map.delete oldPos old }
            else foldEnemies (withNewPos pos enemy { new, old: Map.delete oldPos old })
    Nothing -> new

  -- TODO: Check for blocks
  isFloor :: Point -> Boolean
  isFloor p = case Map.lookup p level.tiles of
    Nothing -> true
    Just CloneMachineButton -> true
    Just Fire -> true
    Just Water -> true
    Just (SwitchableWall Off) -> true
    _ -> false

  goTo :: Point -> Direction -> (Direction -> Enemy) -> Array Direction -> { pos :: Point, enemy :: Enemy }
  goTo pos direction enemy dirs =
    case uncons (filter (isFloor <<< adjustPoint pos) dirs) of
        Just { head: floorDirection } ->
            { pos: adjustPoint pos floorDirection
            , enemy: enemy floorDirection
            }
        Nothing -> { pos, enemy: enemy direction }

  act :: Point -> Enemy -> { pos :: Point, enemy :: Enemy }
  act pos (Bee direction) = goTo pos direction Bee
      [toLeft direction, direction, toRight direction, invert direction]
  act pos (FireBall direction) = goTo pos direction FireBall
      [direction, toRight direction, toLeft direction, invert direction]
  act pos (Tank direction)
    | isFloor (adjustPoint pos direction) = { pos: adjustPoint pos direction, enemy: Tank direction }
    | otherwise = { pos, enemy: Tank direction }
  act pos (Ball direction) = case unit of
    _ | isFloor dest -> { pos: dest, enemy: Ball direction }
    _ | isFloor invertDest -> { pos: invertDest, enemy: Ball invertDir }
    _ -> { pos, enemy: Ball (invert direction) }

    where

    invertDir = invert direction
    invertDest = adjustPoint pos invertDir
    dest = adjustPoint pos direction
