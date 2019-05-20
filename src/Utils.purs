module Utils
  ( Point(..)
  , Direction(..)
  , addIndex
  , adjustPoint
  , foldlM
  , invert
  , toLeft
  , toRight
  , try
  ) where

import Prelude

import Data.Array (mapWithIndex, uncons)
import Data.Maybe (Maybe(..))
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

adjustPoint :: Point -> Direction -> Point
adjustPoint { x, y } direction = case direction of
  Up -> { x, y: y - 1 }
  Left -> { x: x - 1, y }
  Down -> { x, y: y + 1 }
  Right -> { x: x + 1, y }

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

-- | Map a set to the same set but indexed so fst is the index and snd is
-- | element of the set
addIndex :: forall a. Array a -> Array (Tuple Int a)
addIndex a = mapWithIndex Tuple a

try :: forall a. (a -> Boolean) -> (a -> a) -> a -> a
try predicate f v = if predicate v then f v else v

-- | Monadic fold over the elements of a structure,
-- | associating to the left, i.e. from left to right.
foldlM
  :: forall a b m. Applicative m
  => Bind m
  => (a -> b -> m a)
  -> a
  -> Array b
  -> m a
foldlM f i xs = case uncons xs of
  Just { head: x, tail } -> f i x >>= \next -> foldlM f next tail
  Nothing -> pure i
