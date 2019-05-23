module Chip.Action.AI
  ( actAI
  ) where

import Prelude

import Data.Array (uncons, filter)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)

import Chip.Action (ActionResult, inactive)
import Chip.Enemy (Enemy(..))
import Level (Level)
import Utils (Direction, Point, adjustPoint, toLeft, toRight, invert)

actAI :: Level -> ActionResult Level
actAI level = inactive $ level { enemies = actedEnemies }
  where

  actedEnemies = foldEnemies { new: Map.empty, old: level.enemies }

  foldEnemies { new, old } = case Map.findMin old of
    Just { key, value } ->
        let { pos, enemy } = act key value in
        foldEnemies
          { new: Map.insert pos enemy new
          , old: Map.delete key old
          }
    Nothing -> new

  -- TODO: Check for blocks
  isFloor :: Point -> Boolean
  isFloor p = isNothing (Map.lookup p level.tiles)

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
  act pos (FireBall direction)
    = case uncons (filter (isFloor <<< adjustPoint pos) directions) of
      Just { head: floorDirection } ->
          { pos: adjustPoint pos floorDirection
          , enemy: FireBall floorDirection
          }
      Nothing -> { pos, enemy: FireBall direction }

    where

    directions =
        [ direction
        , toRight direction
        , toLeft direction
        , toRight (toRight direction)
        ]
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
