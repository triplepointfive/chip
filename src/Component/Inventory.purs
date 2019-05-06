module Component.Inventory where

import Level (Inventory)

import Data.Maybe (Maybe(..))
import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP

type State = Inventory

data Query a = HandleInput State a

type Input = Inventory

inventoryComponent :: forall m. H.Component HH.HTML Query Input Void m
inventoryComponent =
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
