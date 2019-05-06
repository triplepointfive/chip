module Level where

import Utils

import Data.Array (foldl, replicate, range, zip)
import Data.Map (Map, lookup, empty, insert)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Prelude

mapSize :: Int
mapSize = 32

type Player = { pos :: Point, direction :: Direction }

type Level = { player :: Player, tiles :: Map Point Tile }

data Tile = Wall

movePlayer :: Direction -> Level -> Level
movePlayer direction level = movePlayerTo (adjustPoint level.player.pos direction) level

movePlayerTo :: Point -> Level -> Level
movePlayerTo { x, y } level
  | x < 0 || y < 0 || x >= mapSize || y >= mapSize = level
  | otherwise = case lookup { x, y } level.tiles of
      Just Wall -> level
      Nothing   -> level { player { pos = { x, y } } }

adjustPoint :: Point -> Direction -> Point
adjustPoint { x, y } Up    = { x, y: y - 1 }
adjustPoint { x, y } Left  = { x: x - 1, y }
adjustPoint { x, y } Down  = { x, y: y + 1 }
adjustPoint { x, y } Right = { x: x + 1, y }

initLevel :: Level
initLevel = { player: { pos: { x: 0, y: 0 }, direction: Down }, tiles: empty }

buildLevel :: Array String -> Level
buildLevel =
  foldl
    (\level (Tuple y row) -> foldl (\level (Tuple x c) -> addCell { x: x, y: y } c level) level row)
    initLevel
  <<< addIndex <<< map (addIndex <<< toCharArray)

addCell :: Point -> Char -> Level -> Level
addCell p '#' l = l { tiles = insert p Wall l.tiles}
addCell p '@' l = l { player { pos = p } }
addCell _   _ l = l
