module Level
  ( Action(..)
  , ActionResult(..)
  , Color(..)
  , Level(..)
  , Blank(..)
  , Tile(..)
  , Player(..)
  , Inventory(..)
  , Tiles(..)
  , mapSize
  , build
  , movePlayer
  , visibleHint
  ) where

import Prelude

import Data.Array (foldl, foldr)
import Data.Map (Map, lookup, empty, insert, delete)
import Data.Maybe (Maybe(..))
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))

import Utils (Direction(..), Point, addIndex)

-- | Height and widht of a level grid
mapSize :: Int
mapSize = 32

-- | Everything player can take along
type Inventory =
  { red :: Int
  , cyan :: Int
  , yellow :: Int
  , green :: Boolean
  }

-- | Maping for cell coordinates to object on it.
-- | Does not include floor for simplicity
type Tiles = Map Point Tile

-- | Main character and its data
type Player =
  { pos :: Point
  , direction :: Direction
  }

-- | Represents a floor with all its objects
type Level =
  { player :: Player
  , tiles :: Tiles
  , inventory :: Inventory
  , chipsLeft :: Int
  , hint :: Maybe String
  }

-- | Colors for keys and related doors
data Color
  = Red
  | Cyan
  | Yellow
  | Green

-- | A single cell on a level grid
data Tile
  = Wall
  | Key Color
  | Door Color
  | Chip
  | Socket
  | Exit
  | Hint

data ActionResult
  = CompleteLevel

type Action = Tuple Level (Array ActionResult)

inactive :: Level -> Action
inactive level = Tuple level []

withAction :: Level -> ActionResult -> Action
withAction level action = Tuple level [action]

outOfLevel :: Point -> Boolean
outOfLevel { x, y } = x < 0 || y < 0 || x >= mapSize || y >= mapSize

-- | Tries to move player
movePlayer :: Direction -> Level -> Action
movePlayer direction level =
  if outOfLevel dest
  then inactive $ level
  else case lookup dest level.tiles of
      Nothing           -> inactive $ level { player { pos = dest } }
      Just Chip         -> inactive $ pickChip dest level
      Just (Key color)  -> inactive $ pickUpKey color (level { player { pos = dest } })
      Just (Door color) -> inactive $ openDoor dest color level
      Just Wall         -> inactive $ level
      Just Hint         -> inactive $ level { player { pos = dest } }
      Just Socket       -> inactive $ moveToSocket dest level
      Just Exit         -> withAction (level { player { pos = dest } }) CompleteLevel

  where

  dest :: Point
  dest = adjustPoint level.player.pos direction

moveToSocket :: Point -> Level -> Level
moveToSocket pos level
  | level.chipsLeft == 0 = removeCurrentTile (level { player { pos = pos } })
  | otherwise            = level

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
adjustPoint { x, y } direction = case direction of
  Up -> { x, y: y - 1 }
  Left -> { x: x - 1, y }
  Down -> { x, y: y + 1 }
  Right -> { x: x + 1, y }

-- | Returns hint text if it should be shown
visibleHint :: Level -> Maybe String
visibleHint { tiles, player: { pos }, hint } = case lookup pos tiles of
  Just Hint -> hint
  _ -> Nothing

-- | Structure used to build a level
type Blank =
  { grid :: Array String
  , hint :: Maybe String
  , name :: String
  , chips :: Int
  }

-- | Builds a level from its blank
build :: Blank -> Level
build { grid, hint, chips } =
  foldl
    (\level (Tuple y row) ->
      foldr
        (\(Tuple x c) -> addCell { x: x, y: y } c)
        level
        row
    )
    initLevel
    (addIndex $ map (addIndex <<< toCharArray) grid)

  where

  initLevel :: Level
  initLevel =
    { player: { pos: { x: 0, y: 0 }, direction: Down }
    , tiles: empty
    , inventory: initInventory
    , chipsLeft: chips
    , hint
    }

  initInventory :: Inventory
  initInventory =
    { red: 0
    , cyan: 0
    , yellow: 0
    , green: false
    }

  addCell :: Point -> Char -> Level -> Level
  addCell p c = case c of
    ' ' -> identity
    '#' -> insertTile Wall
    '+' -> insertTile Chip
    'r' -> insertTile (Key Red)
    'c' -> insertTile (Key Cyan)
    'y' -> insertTile (Key Yellow)
    'g' -> insertTile (Key Green)
    'R' -> insertTile (Door Red)
    'C' -> insertTile (Door Cyan)
    'Y' -> insertTile (Door Yellow)
    'G' -> insertTile (Door Green)
    '@' -> _ { player { pos = p } }
    '-' -> insertTile Socket
    '<' -> insertTile Exit
    '?' -> insertTile Hint
    _   -> identity

    where

    insertTile :: Tile -> Level -> Level
    insertTile tile l = l { tiles = insert p tile l.tiles}
