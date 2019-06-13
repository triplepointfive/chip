module Chip.Model.Inventory
  ( Inventory
  , has
  , initInventory
  ) where

import Prelude

import Chip.Model.Color (Color(..))
import Chip.Model.Item (Item(..))

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
