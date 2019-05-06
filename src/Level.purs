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

data Color = Red | Cyan | Yellow | Green

data Tile = Wall | Key Color

movePlayer :: Direction -> Level -> Level
movePlayer direction level = movePlayerTo (adjustPoint level.player.pos direction) level

movePlayerTo :: Point -> Level -> Level
movePlayerTo { x, y } level
  | x < 0 || y < 0 || x >= mapSize || y >= mapSize = level
  | otherwise = case lookup { x, y } level.tiles of
      Just Wall    -> level
      Just (Key _) -> pickUpKey (level { player { pos = { x, y } } })
      Nothing      -> level { player { pos = { x, y } } }

pickUpKey :: Level -> Level
pickUpKey lvl = lvl

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
addCell p ' ' = identity
addCell p '#' = insertTile p Wall
addCell p 'r' = insertTile p (Key Red)
addCell p 'c' = insertTile p (Key Cyan)
addCell p 'y' = insertTile p (Key Yellow)
addCell p 'g' = insertTile p (Key Green)
addCell p '@' = _ { player { pos = p } }
addCell _   _ = identity

insertTile :: Point -> Tile -> Level -> Level
insertTile p tile l = l { tiles = insert p tile l.tiles}
