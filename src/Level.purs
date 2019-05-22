module Level
  ( Action(..)
  , ActionResult(..)
  , DieReason(..)
  , Level(..)
  , Player(..)
  , Tiles(..)
  , enemyAct
  , mapSize
  , movePlayer
  , slide
  , visibleHint
  ) where

import Prelude

import Data.Array (uncons, filter, (:))
import Data.Map (Map, lookup, delete)
import Data.Map as Map
import Data.Set as Set
import Data.Maybe (Maybe(..), isNothing)
import Data.Tuple (Tuple(..))

import Chip.Enemy (Enemy(..))
import Chip.Inventory (Inventory, addItem, has, withdrawKey)
import Chip.Tile (Tile(..), Color, Item(..))
import Utils (Direction, Point, SwitchState(..), try, adjustPoint, toLeft, toRight, invert)

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
  | BlownUp

derive instance eqDieReason :: Eq DieReason

instance showDieReason :: Show DieReason where
  show = case _ of
    Drown -> "Drown"
    Burned -> "Burned"
    Eaten -> "Eaten"
    Timed -> "Timed"
    BlownUp -> "BlownUp"

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
movePlayer :: Boolean -> Direction -> Level -> ActionResult
movePlayer manually direction level = checkForEnemies $ case unit of
  _ | Set.member dest level.blocks -> pushBlock (adjustPoint dest direction)
  _ | outOfLevel dest -> inactive turned
  _ | manually && onIce && not (has SkiSkates level.inventory) -> inactive level
  _ -> move'
  where

  move' = case lookup dest level.tiles of
      Nothing           -> inactive moved
      Just Chip         -> inactive (pickUpChip moved)
      Just (Item item)  -> inactive (pickUp item moved)
      Just (Door color) -> inactive (openDoor color turned)
      Just Wall         -> inactive turned
      -- TODO: Forbid to move on 'em
      Just (Force _)    -> inactive moved
      Just Ice          -> inactive moved
      Just (IceCorner _) -> inactive moved
      Just Bomb -> withAction moved (Die BlownUp)
      Just Water        -> stepInWater moved
      Just Fire         -> stepInFire moved
      Just WallButton -> inactive (toggleWalls moved)
      Just TankButton -> inactive (toggleTanks moved)
      Just (SwitchableWall On) -> inactive turned
      Just (SwitchableWall Off) -> inactive moved
      Just Hint         -> inactive moved
      Just Dirt         -> inactive (removeCurrentTile moved)
      Just Socket       -> inactive $ moveToSocket dest level
      Just Exit         -> withAction moved Complete

  currentTile = lookup level.player.pos level.tiles

  onIce = case currentTile of
    Just Ice -> true
    Just (IceCorner _) -> true
    _ -> false

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

  pickUp :: Item -> Level -> Level
  pickUp item = removeCurrentTile <<< onInventory (addItem item)

  openDoor :: Color -> Level -> Level
  openDoor color =
    try
        (has (Key color) <<< _.inventory)
        (removeCurrentTile <<< onInventory (withdrawKey color) <<< move)

  -- TODO: Check if pushed into a monster
  pushBlock :: Point -> ActionResult
  pushBlock blockDest
    | Set.member blockDest level.blocks = inactive turned
    | otherwise = case lookup blockDest level.tiles of
        Just Water -> movePlayer manually direction $ level
            { tiles = Map.insert blockDest Dirt moved.tiles
            , blocks = Set.delete dest level.blocks
            }
        Nothing -> movePlayer manually direction (moveBlock dest blockDest level)
        Just Hint -> movePlayer manually direction (moveBlock dest blockDest level)
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
  act pos (Tank direction)
    | isFloor (adjustPoint pos direction) = { pos: adjustPoint pos direction, enemy: Tank direction }
    | otherwise = { pos, enemy: Tank direction }

slide :: Level -> ActionResult
slide level = case lookup level.player.pos level.tiles of
  Just (Force direction) | not (has SuctionBoots level.inventory) -> movePlayer false direction level
  Just Ice | not (has SkiSkates level.inventory) -> movePlayer false level.player.direction level
  Just (IceCorner direction) | not (has SkiSkates level.inventory) ->
      if level.player.direction == direction
          then movePlayer false (toRight direction) level
          else movePlayer false (invert direction) level
  _ -> inactive level

toggleTanks :: Level -> Level
toggleTanks level = level { enemies = map toggleTank level.enemies }

  where

  toggleTank :: Enemy -> Enemy
  toggleTank = case _ of
    Tank direction -> Tank (invert direction)
    e -> e

toggleWalls :: Level -> Level
toggleWalls level = level { tiles = map toggleWall level.tiles }

  where

  toggleWall :: Tile -> Tile
  toggleWall = case _ of
    SwitchableWall On -> SwitchableWall Off
    SwitchableWall Off -> SwitchableWall On
    t -> t
