module Chip.Utils
  ( addIndex
  , foldlM
  ) where

import Prelude

import Data.Array (mapWithIndex, uncons)
import Data.Maybe (Maybe(..))

-- | Map a set to the same set but indexed so fst is the index and snd is
-- | element of the set
addIndex :: forall a. Array a -> Array { i :: Int, v :: a }
addIndex a = mapWithIndex (\i v -> { i, v } ) a

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
