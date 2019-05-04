module Main where

import Prelude
import Effect (Effect)
-- import Effect.Console (log)

import Data.Maybe (Maybe(..))
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type State = Boolean

data Query a
  = Toggle a
  | IsOn (Boolean -> a)

type Input = Unit

data Message = Toggled Boolean

data Tile = Floor | Wall

tileClasses :: Tile -> Array String
tileClasses Floor = ["tile", "-floor"]
tileClasses Wall = ["tile", "-wall"]

tileToElem :: forall p i. Tile -> HH.HTML p i
tileToElem tile = HH.span [ HP.classes $ map H.ClassName (tileClasses tile) ] []

type Point = { x :: Int, y :: Int }

data Direction = Up | Down | Left | Right

type Player = { pos :: Point, direction :: Direction }

type Level = { player :: Player }

initLevel = { player: { pos: { x: 0, y: 0 }, direction: Up }}

levelTiles :: Level -> Array (Array Tile)
levelTiles _ =
  [ [Wall, Wall, Wall, Wall, Wall]
  , [Wall, Floor, Floor, Floor, Wall]
  , [Wall, Floor, Floor, Floor, Wall]
  , [Wall, Floor, Floor, Floor, Wall]
  , [Wall, Wall, Wall, Wall, Wall]
  ]

tilesRowElem :: forall p i. Array Tile -> HH.HTML p i
tilesRowElem tiles =
  HH.div
    [ HP.class_ (H.ClassName "level-row") ]
    (map tileToElem tiles)

myButton :: forall m. H.Component HH.HTML Query Input Message m
myButton =
  H.component
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = false

  render :: State -> H.ComponentHTML Query
  render state =
    let
      label = if state then "On" else "Off"
    in
      HH.div []
        (map tilesRowElem (levelTiles initLevel))
      -- HH.button
      --   [ HP.title label
      --   , HE.onClick (HE.input_ Toggle)
      --   ]
      --   [ HH.text label ]

  eval :: Query ~> H.ComponentDSL State Query Message m
  eval = case _ of
    Toggle next -> do
      state <- H.get
      let nextState = not state
      H.put nextState
      H.raise $ Toggled nextState
      pure next
    IsOn reply -> do
      state <- H.get
      pure (reply state)

-- main :: Effect Unit
-- main = do
--   log $ "Hello sailor!" <> show answer

main :: Effect Unit
main = HA.runHalogenAff do
  body <- HA.awaitBody
  runUI myButton unit body
