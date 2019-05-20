module Level
  ( Action(..)
  , ActionResult(..)
  , Blank(..)
  , DieReason(..)
  , Enemy(..)
  , Level(..)
  , Player(..)
  , Tiles(..)
  , build
  , enemyAct
  , hasKey
  , mapSize
  , movePlayer
  , slide
  , visibleHint
  ) where

import Prelude

import Data.Array (foldl, foldr, uncons, filter, (:))
import Data.Map (Map, lookup, delete)
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe(..), isNothing)
import Data.String.CodeUnits (toCharArray)
import Data.Tuple (Tuple(..))

import Chip.Inventory (Inventory, initInventory, addItem, addKey)
import Chip.Tile (Tile(..), Color(..), Item(..))
import Utils (Direction(..), Point, addIndex, try, adjustPoint, toLeft, toRight, invert)

-- | Height and width of a level grid
mapSize :: Int
mapSize = 32

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
  , blocks :: Set.Set Point
  }

data DieReason
  = Drown
  | Burned
  | Eaten
  | Timed

derive instance eqDieReason :: Eq DieReason

instance showDieReason :: Show DieReason where
  show = case _ of
    Drown -> "Drown"
    Burned -> "Burned"
    Eaten -> "Eaten"
    Timed -> "Timed"

data Action
  = Complete
  | Die DieReason

derive instance eqAction :: Eq Action

instance showAction :: Show Action where
  show = case _ of
    Complete -> "Complete"
    Die reason -> "Die " <> show reason

type ActionResult = Tuple Level (Array Action)

inactive :: Level -> ActionResult
inactive level = Tuple level []

withAction :: Level -> Action -> ActionResult
withAction level action = Tuple level [action]

outOfLevel :: Point -> Boolean
outOfLevel { x, y } = x < 0 || y < 0 || x >= mapSize || y >= mapSize

-- | Tries to move player
movePlayer :: Direction -> Level -> ActionResult
movePlayer direction level = checkForEnemies $
  if Set.member dest level.blocks
    then pushBlock (adjustPoint dest direction)
    else
      if outOfLevel dest
      then inactive turned
      else case lookup dest level.tiles of
          Nothing           -> inactive moved
          Just Chip         -> inactive (pickUpChip moved)
          Just (Item item)  -> inactive (pickUp item moved)
          Just (Key color)  -> inactive (pickUpKey color moved)
          Just (Door color) -> inactive (openDoor color turned)
          Just Wall         -> inactive turned
          -- TODO: Forbid to move on 'em
          Just (Force _)    -> inactive moved
          Just Ice          -> inactive moved
          Just (IceCorner _) -> inactive moved
          Just Water        -> stepInWater moved
          Just Fire         -> stepInFire moved
          Just Hint         -> inactive moved
          Just Dirt         -> inactive (removeCurrentTile moved)
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

  pickUp :: Item -> Level -> Level
  pickUp item = removeCurrentTile <<< onInventory (addItem item)

  openDoor :: Color -> Level -> Level
  openDoor color
    = try (hasKey color) (removeCurrentTile <<< withdrawKey color <<< move)

  -- TODO: Check if pushed into a monster
  pushBlock :: Point -> ActionResult
  pushBlock blockDest
    | Set.member blockDest level.blocks = inactive turned
    | otherwise = case lookup blockDest level.tiles of
        Just Water -> inactive $ moved
            { tiles = Map.insert blockDest Dirt moved.tiles
            , blocks = Set.delete dest level.blocks
            }
        Nothing -> inactive (moveBlock dest blockDest moved)
        Just Hint -> inactive (moveBlock dest blockDest moved)
        _ -> inactive turned

stepInWater :: Level -> ActionResult
stepInWater level
  | level.inventory.flippers = inactive level
  | otherwise = withAction level (Die Drown)

stepInFire :: Level -> ActionResult
stepInFire level
  | level.inventory.fireBoots = inactive level
  | otherwise = withAction level (Die Burned)

moveBlock :: Point -> Point -> Level -> Level
moveBlock from to level = level
  { blocks = Set.insert to (Set.delete from level.blocks)
  }

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

checkForEnemies :: ActionResult -> ActionResult
checkForEnemies (Tuple level actions)
  = case lookup level.player.pos level.enemies of
    Just _ -> Tuple level (Die Eaten : actions)
    Nothing -> Tuple level actions

enemyAct :: Level -> ActionResult
enemyAct level = checkForEnemies $ inactive $ level { enemies = actedEnemies }
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

slide :: Level -> ActionResult
slide level = case lookup level.player.pos level.tiles of
  Just (Force direction) -> movePlayer direction level
  Just Ice -> movePlayer level.player.direction level
  Just (IceCorner direction) ->
      if level.player.direction == direction
          then movePlayer (toRight direction) level
          else movePlayer (invert direction) level
  _ -> inactive level

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
    , tiles: Map.empty
    , inventory: initInventory
    , chipsLeft: chips
    , enemies: Map.empty
    , blocks: Set.empty
    , hint
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
    '^' -> insertTile Fire

    '↓' -> insertTile (Force Down)
    '←' -> insertTile (Force Left)
    '↑' -> insertTile (Force Up)
    '→' -> insertTile (Force Right)

    '@' -> _ { player { pos = p } }
    '-' -> insertTile Socket
    '<' -> insertTile Exit
    '?' -> insertTile Hint

    'S' -> insertTile (Item SkiSkates)
    'U' -> insertTile (Item SuctionBoots)
    'I' -> insertTile (Item FireBoots)
    'F' -> insertTile (Item Flippers)

    '╝' -> insertTile (IceCorner Down)
    '╚' -> insertTile (IceCorner Left)
    '╔' -> insertTile (IceCorner Up)
    '╗' -> insertTile (IceCorner Right)
    '╬' -> insertTile Ice

    'O' -> addBlock
    '≈' -> insertTile Dirt
    _   -> identity

    where

    addBlock :: Level -> Level
    addBlock l = l { blocks = Set.insert p l.blocks }

    addEnemy :: Enemy -> Level -> Level
    addEnemy enemy l = l { enemies = Map.insert p enemy l.enemies }

    insertTile :: Tile -> Level -> Level
    insertTile tile l = l { tiles = Map.insert p tile l.tiles}
