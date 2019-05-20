module Chip.Inventory
  ( Inventory
  , addItem
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
