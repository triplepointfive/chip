module Utils
  ( Point(..)
  , Direction(..)
  , addIndex
  ) where

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

-- | Map a set to the same set but indexed so fst is the index and snd is
-- | element of the set
addIndex :: forall a. Array a -> Array (Tuple Int a)
addIndex a = mapWithIndex Tuple a
