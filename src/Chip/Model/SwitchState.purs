module Chip.Model.SwitchState
  ( SwitchState(..)
  ) where

import Prelude

-- | Switch state
data SwitchState
  = On
  | Off

derive instance eqSwitchState :: Eq SwitchState
