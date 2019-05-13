module Component.Inventory
  ( component
  , Query(..)
  , State(..)
  ) where

import Prelude

import Data.Array (singleton)
import Data.Maybe (Maybe, maybe)
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE

import Display (DisplayTile(..), tilesRowElem)
import Level (Inventory, Tile(..), Color(..))

type State =
  { inventory :: Inventory
  , hint :: Maybe String
  }

-- | Accepts current inventory state
data Query a = HandleInput State a

type Input = State

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
  render { inventory: { red, cyan, yellow, green }, hint } =
    HH.div_
      [ HH.div_ (maybe [] (singleton <<< HH.text) hint)
      , tilesRowElem
          [ if red > 0 then Tile (Key Red) else Floor
          , if cyan > 0 then Tile (Key Cyan) else Floor
          , if yellow > 0 then Tile (Key Yellow) else Floor
          , if green then Tile (Key Green) else Floor
          ]
      , tilesRowElem
          [ Floor
          , Floor
          , Floor
          , Floor
          ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Void m
  eval (HandleInput i next) = do
    H.put i
    pure next
