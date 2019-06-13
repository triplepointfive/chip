module Chip.Action.Slide
  ( slide
  ) where

import Prelude

import Data.Map as Map
import Data.Maybe (Maybe(..))

import Chip.Action (Action(..), ActionResult, inactive, Sound(..))
import Chip.Action.MovePlayer (movePlayer)
import Chip.Model (Item(..), Level, Tile(..), has)
import Chip.Mutation (invert, toRight)

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
