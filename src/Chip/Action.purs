module Chip.Action
  ( Action(..)
  , ActionResult(..)
  , Sound(..)
  , addAction
  , inactive
  , withAction
  ) where

import Prelude

import Chip.Model (DieReason)

import Data.Array ((:))

data Sound
  = DoorOpen
  | Oof
  | PickUpItem
  | Bummer
  | PickUpChip
  | LevelComplete
  | Steal
  | Teleported
  | Splash

derive instance eqSound :: Eq Sound

instance showSound :: Show Sound where
  show = case _ of
    DoorOpen -> "DoorOpen"
    Oof -> "Oof"
    PickUpItem -> "PickUpItem"
    Bummer -> "Bummer"
    PickUpChip -> "PickUpChip"
    LevelComplete -> "LevelComplete"
    Steal -> "Steal"
    Teleported -> "Teleported"
    Splash -> "Splash"

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

addAction :: forall a. Action -> ActionResult a -> ActionResult a
addAction action { result, actions } = { result, actions: action : actions }
