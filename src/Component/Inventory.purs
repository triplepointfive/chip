module Component.Inventory
  ( component
  , Query(..)
  , State(..)
  ) where

import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Level (Inventory)

type State = Inventory

-- | Accepts current inventory state
data Query a = HandleInput State a

type Input = Inventory

-- | Component to display current inventory
component :: forall m. H.Component HH.HTML Query Input Void m
component =
  H.component
    { initialState: identity
    , render
    , eval
    , receiver: HE.input HandleInput
    }
  where

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div_ [HH.text (show state)]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (HandleInput i next) = do
    H.put i
    pure next
