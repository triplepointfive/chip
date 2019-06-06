module Chip.Action
  ( Action(..)
  , ActionResult(..)
  , DieReason(..)
  , Sound(..)
  , inactive
  , withAction
  ) where

import Prelude

data DieReason
  = Drown
  | Burned
  | Eaten
  | Timed
  | BlownUp

derive instance eqDieReason :: Eq DieReason

instance showDieReason :: Show DieReason where
  show = case _ of
    Drown -> "Drown"
    Burned -> "Burned"
    Eaten -> "Eaten"
    Timed -> "Timed"
    BlownUp -> "BlownUp"

data Sound
  = DoorOpen
  | Oof
  | PickUpItem
  | Bummer
  | PickUpChip

derive instance eqSound :: Eq Sound

instance showSound :: Show Sound where
  show = case _ of
    DoorOpen -> "DoorOpen"
    Oof -> "Oof"
    PickUpItem -> "PickUpItem"
    Bummer -> "Bummer"
    PickUpChip -> "PickUpChip"

data Action
  = Complete
  | Die DieReason
  | PlaySound Sound

derive instance eqAction :: Eq Action

instance showAction :: Show Action where
  show = case _ of
    Complete -> "Complete"
    Die reason -> "Die " <> show reason
    PlaySound sound -> "PlaySound" <> show sound

type ActionResult a = { result :: a, actions :: Array Action }

inactive :: forall a. a -> ActionResult a
inactive result = { result, actions: [] }

withAction :: forall a. a -> Action -> ActionResult a
withAction result action = { result, actions: [action] }
