module Chip.Inventory
  ( Inventory
  , addItem
  , has
  , initInventory
  ) where

import Prelude

import Chip.Tile (Color(..), Item(..))

-- | Everything player can take along
type Inventory =
  { red :: Int
  , cyan :: Int
  , yellow :: Int
  , green :: Boolean
  , skiSkates :: Boolean
  , suctionBoots :: Boolean
  , fireBoots :: Boolean
  , flippers :: Boolean
  }

-- | Initializes empty inventory
initInventory :: Inventory
initInventory =
  { red: 0
  , cyan: 0
  , yellow: 0
  , green: false
  , skiSkates: false
  , suctionBoots: false
  , fireBoots: false
  , flippers: false
  }

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

-- | Checks whether an item in inventory
has :: Item -> Inventory -> Boolean
has item inv = case item of
  SkiSkates -> inv.skiSkates
  SuctionBoots -> inv.suctionBoots
  FireBoots -> inv.fireBoots
  Flippers -> inv.flippers
  Key color -> case color of
      Red -> inv.red > 0
      Cyan -> inv.cyan > 0
      Yellow -> inv.yellow > 0
      Green -> inv.green
