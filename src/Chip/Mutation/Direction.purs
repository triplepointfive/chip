module Chip.Mutation.Direction
  ( invert
  , toLeft
  , toRight
  ) where

import Chip.Model (Direction(..))

toLeft :: Direction -> Direction
toLeft = case _ of
  Up -> Left
  Left -> Down
  Down -> Right
  Right -> Up

toRight :: Direction -> Direction
toRight = case _ of
  Up -> Right
  Right -> Down
  Down -> Left
  Left -> Up

invert :: Direction -> Direction
invert = case _ of
  Up -> Down
  Down -> Up
  Right -> Left
  Left -> Right
