module Utils
  ( Point(..)
  , Direction(..)
  , addIndex
  , try
  ) where

import Prelude

import Data.Array (mapWithIndex)
import Data.Tuple (Tuple(..))

-- | A 2D point
type Point =
  { x :: Int
  , y :: Int
  }

-- | 4 side direction
data Direction
  = Up
  | Down
  | Left
  | Right

derive instance eqDirection :: Eq Direction

instance showDirection :: Show Direction where
  show = case _ of
    Up -> "Up"
    Down -> "Down"
    Left -> "Left"
    Right -> "Right"

-- | Map a set to the same set but indexed so fst is the index and snd is
-- | element of the set
addIndex :: forall a. Array a -> Array (Tuple Int a)
addIndex a = mapWithIndex Tuple a

try :: forall a. (a -> Boolean) -> (a -> a) -> a -> a
try predicate f v = if predicate v then f v else v
