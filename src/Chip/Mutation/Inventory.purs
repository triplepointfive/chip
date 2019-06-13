module Chip.Mutation.Inventory
  ( addItem
  , withdrawKey
  ) where

import Prelude

import Chip.Model (Item(..), Inventory, Color(..))

-- | Adds an item to inventory
addItem :: Item -> Inventory -> Inventory
addItem item inv = case item of
  SkiSkates -> inv { skiSkates = true }
  SuctionBoots -> inv { suctionBoots = true }
  FireBoots -> inv { fireBoots = true }
  Flippers -> inv { flippers = true }
  Key color -> case color of
      Red -> inv { red = inv.red + 1 }
      Cyan -> inv { cyan = inv.cyan + 1 }
      Yellow -> inv { yellow = inv.yellow + 1 }
      Green -> inv { green = true }

-- | Removes a key from inventory (green is left untouched)
withdrawKey :: Color -> Inventory -> Inventory
withdrawKey color i = case color of
  Red -> i { red = i.red - 1 }
  Cyan -> i { cyan = i.cyan - 1 }
  Yellow -> i { yellow = i.yellow - 1 }
  Green -> i
