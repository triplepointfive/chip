module Level
  ( Level(..)
  , Player(..)
  , Tiles(..)
  , addEnemy
  , checkForEnemies
  , isActiveTrap
  , mapSize
  , movePlayer
  , removeTile
  , slide
  , visibleHint
  ) where

import Prelude

import Data.Array ((:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set

import Chip.Action (Action(..), ActionResult, DieReason(..), inactive, withAction)
import Chip.Enemy (Enemy(..))
import Chip.Inventory (Inventory, addItem, has, withdrawKey, initInventory)
import Chip.Tile (Tile(..), Color, Item(..), WallType(..))
import Utils (Direction, Point, SwitchState(..), try, adjustPoint, toRight, invert)

addEnemy :: Point -> Enemy -> Level -> Level
addEnemy p enemy l = l { enemies = Map.insert p enemy l.enemies }

-- | Height and width of a level grid
mapSize :: Int
mapSize = 32

-- | Mapping for cell coordinates to object on it.
-- | Does not include floor for simplicity
type Tiles = Map.Map Point Tile

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
  , enemies :: Map.Map Point Enemy
  , blocks :: Set.Set Point
  , trapConnections :: Map.Map Point Point
  }

outOfLevel :: Point -> Boolean
outOfLevel { x, y } = x < 0 || y < 0 || x >= mapSize || y >= mapSize

-- TODO: Check for blocks as well
isActiveTrap :: Point -> Level -> Boolean
isActiveTrap point level = case Map.lookup point level.trapConnections of
  _ | Map.lookup point level.tiles /= Just Trap -> false
  Just button | button == level.player.pos -> false
  _ -> true

-- | Tries to move player
movePlayer :: Boolean -> Direction -> Level -> ActionResult Level
movePlayer manually direction level = checkForEnemies $ case unit of
  _ | Map.lookup level.player.pos level.tiles == Just (Wall (Flat direction)) -> inactive turned
  _ | Set.member dest level.blocks -> pushBlock (adjustPoint dest direction)
  _ | outOfLevel dest -> inactive turned
  _ | manually && onIce && not (has SkiSkates level.inventory) -> inactive level
  _ -> move'

  where

  move' = case Map.lookup dest level.tiles of
      Nothing           -> inactive moved
      Just Chip         -> inactive (pickUpChip moved)
      Just (Item item)  -> inactive (pickUp item moved)
      Just (Door color) -> inactive (openDoor color turned)

      Just (Wall Solid) -> inactive turned
      Just (Wall Invisible) -> inactive turned
      Just (Wall Hidden) -> inactive turned { tiles = Map.insert dest (Wall Solid) turned.tiles }
      Just (Wall Blue) -> inactive turned
      Just (Wall Fake) -> inactive (removeCurrentTile moved)
      Just (Wall Recessed) -> inactive moved { tiles = Map.insert dest (Wall Solid) moved.tiles }
      Just (Wall (Flat wallDir)) ->
        if wallDir == invert direction
          then inactive turned
          else inactive moved

      Just Teleport -> case nextTeleport dest direction level of
        Just teleportDest -> inactive turned { player { pos = teleportDest } }
        Nothing -> inactive level { player { direction = invert direction } }
      Just Thief -> inactive moved { inventory = initInventory }
      Just (Force _)    -> inactive moved
      Just Ice          -> inactive moved
      Just (IceCorner _) -> inactive moved
      Just Bomb -> withAction moved (Die BlownUp)
      Just Water        -> stepInWater moved
      Just Fire         -> stepInFire moved
      Just WallButton -> inactive (toggleWalls moved)
      Just TankButton -> inactive (toggleTanks moved)
      -- TODO: apply
      Just CloneMachineButton -> inactive turned
      Just (SwitchableWall On) -> inactive turned
      Just (SwitchableWall Off) -> inactive moved
      Just Hint         -> inactive moved
      Just Dirt         -> inactive (removeCurrentTile moved)
      Just Socket       -> inactive $ moveToSocket dest level
      Just Exit         -> withAction moved Complete
      Just (CloneMachine _) -> inactive turned
      Just Trap -> inactive moved
      Just TrapButton -> inactive moved

  currentTile = Map.lookup level.player.pos level.tiles

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
  pushBlock :: Point -> ActionResult Level
  pushBlock blockDest
    | Set.member blockDest level.blocks = inactive turned
    | otherwise = case Map.lookup blockDest level.tiles of
        Just Water -> movePlayer manually direction $ level
            { tiles = Map.insert blockDest Dirt moved.tiles
            , blocks = Set.delete dest level.blocks
            }
        Nothing -> movePlayer manually direction (moveBlock dest blockDest level)
        Just Hint -> movePlayer manually direction (moveBlock dest blockDest level)
        _ -> inactive turned

stepInWater :: Level -> ActionResult Level
stepInWater level
  | level.inventory.flippers = inactive level
  | otherwise = withAction level (Die Drown)

stepInFire :: Level -> ActionResult Level
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
removeCurrentTile level = removeTile level.player.pos level

onInventory :: (Inventory -> Inventory) -> Level -> Level
onInventory f level = level { inventory = f level.inventory }

removeTile :: Point -> Level -> Level
removeTile point level = level { tiles = Map.delete point level.tiles }

-- | Returns hint text if it should be shown
visibleHint :: Level -> Maybe String
visibleHint { tiles, player: { pos }, hint } = case Map.lookup pos tiles of
  Just Hint -> hint
  _ -> Nothing

checkForEnemies :: ActionResult Level -> ActionResult Level
checkForEnemies { result: level, actions }
  = case Map.lookup level.player.pos level.enemies of
    Just _ -> { result: level, actions: Die Eaten : actions }
    Nothing -> { result: level, actions }

slide :: Level -> ActionResult Level
slide level = case Map.lookup level.player.pos level.tiles of
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

-- TODO: Check for more tile types
isSolid :: Maybe Tile -> Boolean
isSolid = case _ of
  Just (Wall _) -> true
  _ -> false

nextTeleport :: Point -> Direction -> Level -> Maybe Point
nextTeleport origin direction level = iter { x: origin.x - 1, y: origin.y }

  where

  iter { x: -1, y } = iter { x: mapSize - 1, y: y - 1 }
  iter { x, y: -1 } = iter { x: mapSize - 1, y: mapSize - 1 }
  iter dest = case Map.lookup dest level.tiles of
    _ | dest == origin -> Nothing
    Just Teleport | not (isSolid (Map.lookup (adjustPoint dest direction) level.tiles))
      -> Just (adjustPoint dest direction)
    _ -> iter { x: dest.x - 1, y: dest.y }
