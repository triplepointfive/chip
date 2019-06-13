module Level
  ( checkForEnemies
  , mapSize
  , movePlayer
  , slide
  , visibleHint
  ) where

import Prelude

import Data.Array ((:))
import Data.Map as Map
import Data.Maybe (Maybe(..))
import Data.Set as Set

import Chip.Action (Action(..), ActionResult, inactive, withAction, Sound(..), addAction)
import Chip.Model (Point, DieReason(..), Direction, SwitchState(..), Level, Tile(..), Color, Item(..), WallType(..), has, initInventory, isActiveTrap)
import Chip.Mutation (addItem, withdrawKey, toRight, invert, adjustPoint, removeCurrentTile, moveBlock, onInventory, countChip, moveToSocket, toggleTanks, toggleWalls)

-- | Height and width of a level grid
mapSize :: Int
mapSize = 32

outOfLevel :: Point -> Boolean
outOfLevel { x, y } = x < 0 || y < 0 || x >= mapSize || y >= mapSize

sound :: Sound -> Level -> ActionResult Level
sound effect level = withAction level (PlaySound effect)

-- | Tries to move player
movePlayer :: Boolean -> Direction -> Level -> ActionResult Level
movePlayer manually direction level =
  movePlayerTo manually direction (adjustPoint level.player.pos direction) level

movePlayerTo :: Boolean -> Direction -> Point -> Level -> ActionResult Level
movePlayerTo manually direction dest level =
  checkForEnemies $ case { from: Map.lookup level.player.pos level.tiles, to: Map.lookup dest level.tiles } of
  { from: Just Trap } | isActiveTrap level.player.pos level
      -> sound Oof turned
  { from: Just (Wall (Flat flatDir)) } | flatDir == direction
      -> sound Oof turned
  _ | Set.member dest level.blocks
      -> pushBlock (adjustPoint dest direction)
  { to: Nothing } | outOfLevel dest
      -> sound Oof turned
  _ | manually && onIce && not (has SkiSkates level.inventory)
      -> inactive level

  { to: Nothing }
      -> inactive moved
  { to: Just Chip }
      -> withAction (pickUpChip moved) (PlaySound PickUpChip)
  { to: Just (Item item) }
      -> withAction (pickUp item moved) (PlaySound PickUpItem)

  { to: Just (Door color) } | has (Key color) level.inventory
      -> withAction (openDoor color turned) (PlaySound DoorOpen)
  { to: Just (Door _) }
      -> withAction turned (PlaySound Oof)

  { to: Just (Wall Solid) }
      -> sound Oof turned
  { to: Just (Wall Invisible) }
      -> sound Oof turned
  { to: Just (Wall Hidden) }
      -> sound Oof turned { tiles = Map.insert dest (Wall Solid) turned.tiles }
  { to: Just (Wall Blue) }
      -> sound Oof turned { tiles = Map.insert dest (Wall Solid) turned.tiles }

  { to: Just (Wall Fake) }
      -> inactive (removeCurrentTile moved)
  { to: Just (Wall Recessed) }
      -> inactive moved { tiles = Map.insert dest (Wall Solid) moved.tiles }

  { to: Just (Wall (Flat wallDir)) } ->
    if wallDir == invert direction
      then inactive turned
      else inactive moved

  { to: Just Teleport } -> case nextTeleport true dest direction level of
    Just teleportDest -> addAction (PlaySound Teleported) (movePlayerTo manually direction teleportDest level)
    Nothing -> sound Teleported level { player { direction = invert direction } }

  { to: Just Thief }
      -> sound Steal (moved { inventory = initInventory })
  { to: Just (Force _) }
      -> inactive moved
  { to: Just Ice }
      -> inactive moved
  { to: Just (IceCorner _) }
      -> inactive moved
  { to: Just Bomb }
      -> withAction moved (Die BlownUp)
  { to: Just Water }
      -> stepInWater moved
  { to: Just Fire }
      -> stepInFire moved
  { to: Just WallButton }
      -> inactive (toggleWalls moved)
  { to: Just TankButton }
      -> inactive (toggleTanks moved)

  -- TODO: apply
  { to: Just CloneMachineButton }
      -> inactive turned
  { to: Just (SwitchableWall On) }
      -> inactive turned
  { to: Just (SwitchableWall Off) }
      -> inactive moved
  { to: Just Hint }
      -> inactive moved
  { to: Just Dirt }
      -> inactive (removeCurrentTile moved)
  { to: Just Gravel }
      -> inactive moved
  { to: Just Socket }
      -> inactive $ moveToSocket dest level
  { to: Just Exit }
      -> withAction moved Complete
  { to: Just (CloneMachine _) }
      -> inactive turned
  { to: Just Trap }
      -> inactive moved
  { to: Just TrapButton }
      -> inactive moved

  where

  currentTile = Map.lookup level.player.pos level.tiles

  onIce = case currentTile of
    Just Ice -> true
    Just (IceCorner _) -> true
    _ -> false

  openDoor :: Color -> Level -> Level
  openDoor color = removeCurrentTile <<< onInventory (withdrawKey color) <<< move

  turned :: Level
  turned = level { player
    { direction = direction
    , turnedAt = level.tick
    , movedAt = if manually then level.tick else level.player.movedAt
    } }

  moved :: Level
  moved = turned { player { pos = dest } }

  move :: Level -> Level
  move = _ { player { pos = dest } }

  pickUpChip :: Level -> Level
  pickUpChip = countChip <<< removeCurrentTile

  pickUp :: Item -> Level -> Level
  pickUp item = removeCurrentTile <<< onInventory (addItem item)

  -- TODO: Check if pushed into a monster
  pushBlock :: Point -> ActionResult Level
  pushBlock blockDest = case Map.lookup blockDest level.tiles of
    _ | Set.member blockDest level.blocks -> inactive turned
    _ | Map.member blockDest level.enemies -> inactive turned
    Just Bomb -> movePlayer manually direction $ level
        { tiles = Map.delete blockDest level.tiles
        , blocks = Set.delete dest level.blocks
        }
    Just Teleport -> case nextTeleport false blockDest direction level of
      Just teleportDest -> addAction (PlaySound Teleported)
        (movePlayerTo manually direction dest (moveBlock dest teleportDest level))
      Nothing -> sound Oof turned
    Just Water -> addAction (PlaySound Splash) $ movePlayer manually direction $ level
        { tiles = Map.insert blockDest Dirt moved.tiles
        , blocks = Set.delete dest level.blocks
        }
    Nothing -> movePlayer manually direction (moveBlock dest blockDest level)
    Just Hint -> movePlayer manually direction (moveBlock dest blockDest level)
    Just TrapButton -> movePlayer manually direction (moveBlock dest blockDest level)
    _ -> inactive turned

stepInWater :: Level -> ActionResult Level
stepInWater level
  | level.inventory.flippers = inactive level
  | otherwise = withAction level (Die Drown)

stepInFire :: Level -> ActionResult Level
stepInFire level
  | level.inventory.fireBoots = inactive level
  | otherwise = withAction level (Die Burned)


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
  Just (Force direction) | not (has SuctionBoots level.inventory) ->
    movePlayer false direction level
  Just Ice | not (has SkiSkates level.inventory) ->
    case movePlayer false level.player.direction level of
      { result, actions: [PlaySound Oof] } | result.player.pos == level.player.pos
          -> slide level { player { direction = invert level.player.direction } }
      res -> res
  Just (IceCorner direction) | not (has SkiSkates level.inventory) ->
      if level.player.direction == direction
          then movePlayer false (toRight direction) level
          else movePlayer false (invert direction) level
  _ -> inactive level

nextTeleport :: Boolean -> Point -> Direction -> Level -> Maybe Point
nextTeleport canPush origin direction { tiles, blocks, enemies } =
  iter { x: origin.x - 1, y: origin.y }

  where

  iter { x: -1, y } = iter { x: mapSize - 1, y: y - 1 }
  iter { x, y: -1 } = iter { x: mapSize - 1, y: mapSize - 1 }
  iter dest = case Map.lookup dest tiles of
    _ | dest == origin -> Nothing
    Just Teleport | not (isSolid (adjustPoint dest direction))
      -> Just (adjustPoint dest direction)
    _ -> iter { x: dest.x - 1, y: dest.y }

  isSolid :: Point -> Boolean
  isSolid pos = case Map.lookup pos tiles of
    Just (Wall _) -> true
    _ | Set.member pos blocks -> not canPush
    _ | Map.member pos enemies -> true
    _ -> false
