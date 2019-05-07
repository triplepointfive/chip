module Level where

import Utils

import Data.Array (foldl, replicate, range, zip)
import Data.Map (Map, lookup, empty, insert, delete)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))
import Prelude

mapSize :: Int
mapSize = 32

type Inventory = { red :: Int, cyan :: Int, yellow :: Int, green :: Boolean }

type Tiles = Map Point Tile

type Player = { pos :: Point, direction :: Direction }

type Level = { player :: Player, tiles :: Tiles, inventory :: Inventory }

data Color = Red | Cyan | Yellow | Green

data Tile = Wall | Key Color | Door Color

movePlayer :: Direction -> Level -> Level
movePlayer direction level = movePlayerTo (adjustPoint level.player.pos direction) level

movePlayerTo :: Point -> Level -> Level
movePlayerTo { x, y } level
  | x < 0 || y < 0 || x >= mapSize || y >= mapSize = level
  | otherwise = case lookup { x, y } level.tiles of
      Just Wall     -> level
      Just (Key c)  -> pickUpKey c (level { player { pos = { x, y } } })
      Just (Door _) -> level
      Nothing       -> level { player { pos = { x, y } } }

pickUpKey :: Color -> Level -> Level
pickUpKey color level =
  level
    { tiles = removeTile level.player.pos level.tiles
    , inventory = addKey color level.inventory
    }

addKey :: Color -> Inventory -> Inventory
addKey Red    inv = inv { red = inv.red + 1 }
addKey Cyan   inv = inv { cyan = inv.cyan + 1 }
addKey Yellow inv = inv { yellow = inv.yellow + 1 }
addKey Green  inv = inv { green = true }

removeTile :: Point -> Tiles -> Tiles
removeTile pos = delete pos

adjustPoint :: Point -> Direction -> Point
adjustPoint { x, y } Up    = { x, y: y - 1 }
adjustPoint { x, y } Left  = { x: x - 1, y }
adjustPoint { x, y } Down  = { x, y: y + 1 }
adjustPoint { x, y } Right = { x: x + 1, y }

initInventory :: Inventory
initInventory =
  { red: 0
  , cyan: 0
  , yellow: 0
  , green: false }

initLevel :: Level
initLevel =
  { player: { pos: { x: 0, y: 0 }, direction: Down }
  , tiles: empty
  , inventory: initInventory
  }

build :: Array String -> Level
build =
  foldl
    (\level (Tuple y row) ->
      foldl
        (\level (Tuple x c) -> addCell { x: x, y: y } c level)
        level
        row
    )
    initLevel
  <<< addIndex <<< map (addIndex <<< toCharArray)

addCell :: Point -> Char -> Level -> Level
addCell p ' ' = identity
addCell p '#' = insertTile p Wall
addCell p 'r' = insertTile p (Key Red)
addCell p 'c' = insertTile p (Key Cyan)
addCell p 'y' = insertTile p (Key Yellow)
addCell p 'g' = insertTile p (Key Green)
addCell p 'R' = insertTile p (Door Red)
addCell p 'C' = insertTile p (Door Cyan)
addCell p 'Y' = insertTile p (Door Yellow)
addCell p 'G' = insertTile p (Door Green)
addCell p '@' = _ { player { pos = p } }
addCell _   _ = identity

insertTile :: Point -> Tile -> Level -> Level
insertTile p tile l = l { tiles = insert p tile l.tiles}
