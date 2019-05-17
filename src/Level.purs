module Level
  ( Action(..)
  , ActionResult(..)
  , Blank(..)
  , Color(..)
  , Enemy(..)
  , Inventory(..)
  , Level(..)
  , Player(..)
  , Tile(..)
  , Tiles(..)
  , build
  , enemyAct
  , hasKey
  , mapSize
  , movePlayer
  , visibleHint
  ) where

import Prelude

import Data.Array (foldl, foldr, uncons, filter)
import Data.Map (Map, lookup, empty, insert, delete)
import Data.Map as Map
import Data.Maybe (Maybe(..), isNothing)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))

import Utils (Direction(..), Point, addIndex, try, adjustPoint, toLeft, toRight)

-- | Height and width of a level grid
mapSize :: Int
mapSize = 32

-- | Everything player can take along
type Inventory =
  { red :: Int
  , cyan :: Int
  , yellow :: Int
  , green :: Boolean
  }

-- | Mapping for cell coordinates to object on it.
-- | Does not include floor for simplicity
type Tiles = Map Point Tile

-- | Main character and its data
type Player =
  { pos :: Point
  , direction :: Direction
  }

data Enemy
  = Bee Direction

-- | Represents a floor with all its objects
type Level =
  { player :: Player
  , tiles :: Tiles
  , inventory :: Inventory
  , chipsLeft :: Int
  , hint :: Maybe String
  , enemies :: Map Point Enemy
  }

-- | Colors for keys and related doors
data Color
  = Red
  | Cyan
  | Yellow
  | Green

derive instance eqColor :: Eq Color

-- | A single cell on a level grid
data Tile
  = Wall
  | Key Color
  | Door Color
  | Chip
  | Socket
  | Exit
  | Hint
  | Water

derive instance eqTile :: Eq Tile

instance showTile :: Show Tile where
  show = case _ of
    Wall -> "#"
    Key color -> case color of
      Red -> "r"
      Cyan -> "c"
      Yellow -> "y"
      Green -> "g"
    Door color -> case color of
      Red -> "R"
      Cyan -> "C"
      Yellow -> "Y"
      Green -> "G"
    Chip -> "+"
    Socket -> "-"
    Exit -> "<"
    Hint -> "?"
    Water -> "~"

data Action
  = Complete
  | Die String

derive instance eqAction :: Eq Action

instance showAction :: Show Action where
  show = case _ of
    Complete -> "Complete"
    Die reason -> "Die " <> reason

type ActionResult = Tuple Level (Array Action)

inactive :: Level -> ActionResult
inactive level = Tuple level []

withAction :: Level -> Action -> ActionResult
withAction level action = Tuple level [action]

outOfLevel :: Point -> Boolean
outOfLevel { x, y } = x < 0 || y < 0 || x >= mapSize || y >= mapSize

-- | Tries to move player
movePlayer :: Direction -> Level -> ActionResult
movePlayer direction level =
  if outOfLevel dest
  then inactive turned
  else case lookup dest level.tiles of
      Nothing           -> inactive moved
      Just Chip         -> inactive (pickUpChip moved)
      Just (Key color)  -> inactive (pickUpKey color moved)
      Just (Door color) -> inactive (openDoor color turned)
      Just Wall         -> inactive turned
      Just Water        -> withAction moved (Die "Ooops! Chip can't swim without flippers!")
      Just Hint         -> inactive moved
      Just Socket       -> inactive $ moveToSocket dest level
      Just Exit         -> withAction moved Complete

  where

  turned :: Level
  turned = level { player { direction = direction } }

  moved :: Level
  moved = turned { player { pos = dest } }

  move :: Level -> Level
  move = _ { player { pos = dest } }

  dest :: Point
  dest = adjustPoint level.player.pos direction

  pickUpChip :: Level -> Level
  pickUpChip = countChip <<< removeCurrentTile

  pickUpKey :: Color -> Level -> Level
  pickUpKey color = removeCurrentTile <<< onInventory (addKey color)

  openDoor :: Color -> Level -> Level
  openDoor color
    = try (hasKey color) (removeCurrentTile <<< withdrawKey color <<< move)

moveToSocket :: Point -> Level -> Level
moveToSocket pos level
  | level.chipsLeft == 0 = removeCurrentTile (level { player { pos = pos } })
  | otherwise            = level

countChip :: Level -> Level
countChip level
  | level.chipsLeft == 0 = level
  | otherwise            = level { chipsLeft = level.chipsLeft - 1 }

withdrawKey :: Color -> Level -> Level
withdrawKey color l = case color of
  Red -> l { inventory { red = l.inventory.red - 1 } }
  Cyan -> l { inventory { cyan = l.inventory.cyan - 1 } }
  Yellow -> l { inventory { yellow = l.inventory.yellow - 1 } }
  Green -> l

hasKey :: Color -> Level -> Boolean
hasKey color { inventory: { red, cyan, yellow, green } } = case color of
  Red -> red > 0
  Cyan -> cyan > 0
  Yellow -> yellow > 0
  Green -> green

removeCurrentTile :: Level -> Level
removeCurrentTile l = l { tiles = removeTile l.player.pos l.tiles }

onInventory :: (Inventory -> Inventory) -> Level -> Level
onInventory f level = level { inventory = f level.inventory }

addKey :: Color -> Inventory -> Inventory
addKey color inv = case color of
  Red -> inv { red = inv.red + 1 }
  Cyan -> inv { cyan = inv.cyan + 1 }
  Yellow -> inv { yellow = inv.yellow + 1 }
  Green -> inv { green = true }

removeTile :: Point -> Tiles -> Tiles
removeTile pos = delete pos

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
  , timeLimit :: Int
  }

enemyAct :: Level -> Level
enemyAct level = level { enemies = actedEnemies }
  where

  actedEnemies = foldEnemies { new: empty, old: level.enemies }

  foldEnemies { new, old } = case Map.findMin old of
    Just { key, value } ->
        let { pos, enemy } = act key value in
        foldEnemies
          { new: Map.insert pos enemy new
          , old: Map.delete key old
          }
    Nothing -> new

  isFloor :: Point -> Boolean
  isFloor p = isNothing (lookup p level.tiles)

  act :: Point -> Enemy -> { pos :: Point, enemy :: Enemy }
  act pos (Bee direction)
    = case uncons (filter (isFloor <<< adjustPoint pos) directions) of
      Just { head: floorDirection } ->
          { pos: adjustPoint pos floorDirection
          , enemy: Bee floorDirection
          }
      Nothing -> { pos, enemy: Bee direction }

    where

    directions =
        [ toLeft direction
        , direction
        , toRight direction
        , toRight (toRight direction)
        ]

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
    , enemies: empty
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
    'b' -> addEnemy (Bee Up)
    '~' -> insertTile Water
    '@' -> _ { player { pos = p } }
    '-' -> insertTile Socket
    '<' -> insertTile Exit
    '?' -> insertTile Hint
    _   -> identity

    where

    addEnemy :: Enemy -> Level -> Level
    addEnemy enemy l = l { enemies = insert p enemy l.enemies }

    insertTile :: Tile -> Level -> Level
    insertTile tile l = l { tiles = insert p tile l.tiles}
