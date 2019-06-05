module Game
  ( Game(..)
  , State(..)
  , onLevel
  , tick
  ) where

import Prelude

import Chip.Action (DieReason)
import Level (Level)
import Utils (Direction(..))

data State
  = Init
  | Alive
  | Dead DieReason
  | Pause
  | Complete

derive instance eqState :: Eq State

type Game =
  { level :: Level
  , levelNum :: Int
  , state :: State
  , name :: String
  }

tick :: Game -> Game
tick = onLevel (\l -> turnPlayer l { ticksLeft = l.ticksLeft - 1 })

onLevel :: (Level -> Level) -> Game -> Game
onLevel f game = game { level = f game.level }

turnPlayer :: Level -> Level
turnPlayer l
  | l.ticksLeft < l.player.turnedAt - 4 = l { player { direction = Down } }
  | otherwise = l
