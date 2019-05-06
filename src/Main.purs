module Main where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)

import Data.Map (Map, lookup, empty, insert)
import Data.Array (mapWithIndex, foldl, replicate, range)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Tuple (Tuple(..))
import Data.String.CodeUnits (toCharArray)
import Halogen as H
import Halogen.Aff as HA
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Halogen.Query.EventSource as ES
import Halogen.VDom.Driver (runUI)
import Keyboard as K
import Web.Event.Event as E
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import Web.UIEvent.KeyboardEvent (KeyboardEvent)
import Web.UIEvent.KeyboardEvent as KE

mapSize :: Int
mapSize = 32

type State = Level

data Query a
  = Init a
  | Move Direction a
  | HandleKey KeyboardEvent (H.SubscribeStatus -> a)

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

buildLevel :: Array String -> Level
buildLevel =
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
levelTiles lvl = mapWithIndex (\y r -> map (\x -> buildTile { x: x, y: y } lvl) r) (replicate mapSize (range 0 (mapSize - 1)))

buildTile :: Point -> Level -> Tile
buildTile p { player: { pos }, tiles }
  | p == pos  = Boy Down
  | otherwise = fromMaybe Floor (lookup p tiles)

tilesRowElem :: forall p i. Array Tile -> HH.HTML p i
tilesRowElem tiles =
  HH.div
    [ HP.class_ (H.ClassName "level-row") ]
    (map tileToElem tiles)

movePlayer :: Direction -> Level -> Level
movePlayer direction level = movePlayerTo (adjustPoint level.player.pos direction) level

movePlayerTo :: Point -> Level -> Level
movePlayerTo { x, y } level
  | x < 0 || y < 0 || x >= mapSize || y >= mapSize = level
  | otherwise                            = level { player { pos = { x, y } } }

adjustPoint :: Point -> Direction -> Point
adjustPoint { x, y } Up    = { x, y: y - 1 }
adjustPoint { x, y } Left  = { x: x - 1, y }
adjustPoint { x, y } Down  = { x, y: y + 1 }
adjustPoint { x, y } Right = { x: x + 1, y }

myButton :: forall m. H.Component HH.HTML Query Input Message Aff
myButton =
  H.lifecycleComponent
    { initialState: const initialState
    , render
    , eval
    , initializer: Just (H.action Init)
    , finalizer: Nothing
    , receiver: const Nothing
    }
  where

  initialState :: State
  initialState = buildLevel lvl1

  render :: State -> H.ComponentHTML Query
  render state =
    HH.div []
      [ HH.div [] (map tilesRowElem (levelTiles state))
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Down)) ] [ HH.text "D" ]
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Left)) ] [ HH.text "L" ]
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Up)) ] [ HH.text "U" ]
      , HH.button [ HP.title "u" , HE.onClick (HE.input_ (Move Right)) ] [ HH.text "R" ]
      ]

  eval :: Query ~> H.ComponentDSL State Query Message Aff
  eval (Init next) = do
    document <- H.liftEffect $ DOM.document =<< DOM.window
    H.subscribe $ ES.eventSource' (K.onKeyUp document) (Just <<< H.request <<< HandleKey)
    pure next
  eval (Move direction next) = do
    state <- H.get
    let nextState = movePlayer direction state
    H.put nextState
    -- H.raise $ Toggled nextState
    pure next
  eval (HandleKey ev reply)
    | KE.key ev == "ArrowDown"  = moveEvent ev reply Down
    | KE.key ev == "ArrowLeft"  = moveEvent ev reply Left
    | KE.key ev == "ArrowUp"    = moveEvent ev reply Up
    | KE.key ev == "ArrowRight" = moveEvent ev reply Right
    | otherwise =
        pure (reply H.Listening)

moveEvent ev reply direction = do
  H.liftEffect $ E.preventDefault (KE.toEvent ev)
  H.modify_ (movePlayer direction)
  pure (reply H.Listening)

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
