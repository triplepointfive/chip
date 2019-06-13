-- Legally stolen from here https://github.com/bodil/purescript-is-magic
module Chip.Lib.Sound
  ( SoundEffect(..)
  , URL
  , Volume
  , play
  ) where

import Prelude

import Effect (Effect)

foreign import data SOUND :: Type

type Volume = Number
type URL = String

-- | The various types of sound effect.
-- |
-- | * `Quiet` means no sound effect.
-- | * `Sound` plays a sound effect once at a given volume.
-- | * `ExclusiveSound` causes other running sound effects to
-- |    stop before it starts playing.
-- | * `RepeatSound` plays a looping sound effect.
data SoundEffect = Quiet
                 | Sound URL Volume
                 | ExclusiveSound URL Volume
                 | RepeatSound URL Volume

-- | The sound engine is an external JavaScript function.
foreign import play' :: (SoundEffect -> String) -> SoundEffect -> Effect Unit


-- | A helper function to allow the external JS code to figure out which type
-- | of `SoundEffect` it's been given.
effectType :: SoundEffect -> String
effectType Quiet = "quiet"
effectType (Sound _ _) = "sound"
effectType (ExclusiveSound _ _) = "exclusiveSound"
effectType (RepeatSound _ _) = "repeatSound"

-- | Play a `SoundEffect`.
play :: SoundEffect -> Effect Unit
play = play' effectType
