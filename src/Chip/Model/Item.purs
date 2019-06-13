module Chip.Model.Item
  ( Item(..)
  ) where

import Prelude

import Chip.Model.Color (Color(..))

-- | What can be found on floor and picked up
data Item
  = SkiSkates
  | SuctionBoots
  | FireBoots
  | Flippers
  | Key Color

derive instance eqItem :: Eq Item

instance showItem :: Show Item where
  show = case _ of
    SkiSkates -> "S"
    SuctionBoots -> "U"
    FireBoots -> "I"
    Flippers -> "F"
    Key color -> case color of
      Red -> "r"
      Cyan -> "c"
      Yellow -> "y"
      Green -> "g"
