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

type Level =
  { player :: Player
  , tiles :: Tiles
  , inventory :: Inventory
  , chipsLeft :: Int
  }

data Color = Red | Cyan | Yellow | Green

data Tile = Wall | Key Color | Door Color | Chip

movePlayer :: Direction -> Level -> Level
movePlayer direction level =
  movePlayerTo (adjustPoint level.player.pos direction) level

movePlayerTo :: Point -> Level -> Level
movePlayerTo { x, y } level
  | x < 0 || y < 0 || x >= mapSize || y >= mapSize = level
  | otherwise = case lookup { x, y } level.tiles of
      Just Wall         -> level
      Just Chip         -> pickChip { x, y } level
      Just (Key color)  -> pickUpKey color (level { player { pos = { x, y } } })
      Just (Door color) -> openDoor { x, y } color level
      Nothing           -> level { player { pos = { x, y } } }

pickChip :: Point -> Level -> Level
pickChip pos level = countChip (removeCurrentTile (level { player { pos = pos } }))

openDoor :: Point -> Color -> Level -> Level
openDoor pos color level
  | hasKey color level.inventory = removeCurrentTile (withdrawKey color (level { player { pos = pos } }))
  | otherwise                    = level

countChip :: Level -> Level
countChip level
  | level.chipsLeft == 0 = level
  | otherwise            = level { chipsLeft = level.chipsLeft - 1 }

withdrawKey :: Color -> Level -> Level
withdrawKey Red l    = l { inventory { red = l.inventory.red - 1 } }
withdrawKey Cyan l   = l { inventory { cyan = l.inventory.cyan - 1 } }
withdrawKey Yellow l = l { inventory { yellow = l.inventory.yellow - 1 } }
withdrawKey Green l  = l

hasKey :: Color -> Inventory -> Boolean
hasKey Red { red }       = red > 0
hasKey Cyan { cyan }     = cyan > 0
hasKey Yellow { yellow } = yellow > 0
hasKey Green { green }   = green

removeCurrentTile :: Level -> Level
removeCurrentTile l = l { tiles = removeTile l.player.pos l.tiles }

pickUpKey :: Color -> Level -> Level
pickUpKey color level = removeCurrentTile $
  level { inventory = addKey color level.inventory }

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
  , chipsLeft: 11
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
addCell p '+' = insertTile p Chip
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
