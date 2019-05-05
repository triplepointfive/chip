module Main where

import Prelude
import Effect (Effect)
-- import Effect.Console (log)

import Data.Map (Map, lookup, empty, insert)
import Data.Array (mapWithIndex, foldl, replicate, range)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
-- import Control.Semigroupoid ((<<<))
import Data.String.CodeUnits (toCharArray)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
-- import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.VDom.Driver (runUI)

type State = Boolean

data Query a
  = Toggle a
  | IsOn (Boolean -> a)

type Input = Unit

data Message = Toggled Boolean

data Tile = Floor | Wall | Boy Direction

tileClasses :: Tile -> Array String
tileClasses Floor = ["tile", "-floor"]
tileClasses Wall = ["tile", "-wall"]
tileClasses (Boy Down) = ["tile", "-boy", "-down"]
tileClasses (Boy Left) = ["tile", "-boy", "-left"]
tileClasses (Boy Up) = ["tile", "-boy", "-up"]
tileClasses (Boy Right) = ["tile", "-boy", "-right"]

tileToElem :: forall p i. Tile -> HH.HTML p i
tileToElem tile = HH.span [ HP.classes $ map H.ClassName (tileClasses tile) ] []

type Point = { x :: Int, y :: Int }

data Direction = Up | Down | Left | Right

type Player = { pos :: Point, direction :: Direction }

type Level = { player :: Player, tiles :: Map Point Tile }

buildLevel' :: Array String -> Level
buildLevel' =
  foldl
    (\level (Tuple y row) -> foldl (\level (Tuple x c) -> addCell { x: x, y: y } c level) level row)
    initLevel
  <<< addIndex <<< map (addIndex <<< toCharArray)

addCell :: Point -> Char -> Level -> Level
addCell p '#' l = l { tiles = insert p Wall l.tiles}
addCell p '@' l = l { player { pos = p } }
addCell _   _ l = l

-- | Map a set to the same set but indexed so fst is the index and snd is
-- | element of the set.
addIndex :: forall a. Array a -> Array (Tuple Int a)
addIndex a = mapWithIndex Tuple a

initLevel :: Level
initLevel = { player: { pos: { x: 0, y: 0 }, direction: Down }, tiles: empty }

levelTiles :: Level -> Array (Array Tile)
levelTiles lvl = mapWithIndex (\y r -> map (\x -> buildTile { x: x, y: y } lvl) r) (replicate 32 (range 0 31))

buildTile :: Point -> Level -> Tile
buildTile p { player: { pos }, tiles }
  | p == pos  = Boy Down
  | otherwise = fromMaybe Floor (lookup p tiles)

tilesRowElem :: forall p i. Array Tile -> HH.HTML p i
tilesRowElem tiles =
  HH.div
    [ HP.class_ (H.ClassName "level-row") ]
    (map tileToElem tiles)

lvl1_ :: Level
lvl1_ = buildLevel' lvl1

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
        (map tilesRowElem (levelTiles lvl1_))
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



lvl1 :: Array String
lvl1 =
  [ "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "          ##### #####           "
  , "          #   ###   #           "
  , "          #   # #   #           "
  , "        ##### # # #####         "
  , "        #             #         "
  , "        #   #     #   #         "
  , "        #####  @  #####         "
  , "        #   #     #   #         "
  , "        #             #         "
  , "        ###### # ######         "
  , "            #  #  #             "
  , "            #  #  #             "
  , "            #  #  #             "
  , "            #######             "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  , "                                "
  ]
