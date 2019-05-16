module Display
  ( DisplayTile(..)
  , levelTiles
  , tilesRowElem
  ) where

import Prelude

import Data.Array (range)
import Data.Map (lookup)
import Data.Maybe (maybe, Maybe(..))
import Data.Tuple (Tuple(..))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

import Level (Color(..), Level, Tile(..), mapSize)
import Utils (Direction(..), Point)

-- | What shall be output on screen
data DisplayTile
  = Floor
  | Tile Tile
  | Boy Direction
  | Swimming Direction

-- | Builds a matrix of `DisplayTile` with `radius` * 2 + 1 size.
-- | This function transforms raw level data to what shall be
-- | presented in specific cell
levelTiles :: Int -> Level -> Array (Array DisplayTile)
levelTiles radius level =
  map
    (\(Tuple y row) -> map (\x -> buildTile { x: x, y: y } level) row)
    (rangeSlice radius level.player.pos)

rangeSlice :: Int -> Point ->  Array (Tuple Int (Array Int))
rangeSlice r { x, y } =
  map (flip Tuple xRange) (range (from y) (d + from y - 1))

  where

  xRange = range (from x) (d + from x - 1)
  d = (2 * r) + 1
  from c = min (max 0 (c - r)) (mapSize - d)

tileClasses :: DisplayTile -> String
tileClasses = case _ of
  Floor -> "tile -floor"
  Tile Wall -> "tile -wall"
  Tile Chip -> "tile -chip"
  Tile (Key Red) -> "tile -key -red"
  Tile (Key Cyan) -> "tile -key -cyan"
  Tile (Key Yellow) -> "tile -key -yellow"
  Tile (Key Green) -> "tile -key -green"
  Tile (Door Red) -> "tile -door -red"
  Tile (Door Cyan) -> "tile -door -cyan"
  Tile (Door Yellow) -> "tile -door -yellow"
  Tile (Door Green) -> "tile -door -green"
  Boy Down -> "tile -boy -down"
  Boy Left -> "tile -boy -left"
  Boy Up -> "tile -boy -up"
  Boy Right -> "tile -boy -right"
  Swimming Down -> "tile -boy -down -swimming"
  Swimming Left -> "tile -boy -left -swimming"
  Swimming Up -> "tile -boy -up -swimming"
  Swimming Right -> "tile -boy -right -swimming"
  Tile Water -> "tile -water"
  Tile Exit -> "tile -exit"
  Tile Hint -> "tile -hint"
  Tile Socket -> "tile -socket"

tileToElem :: forall p i. DisplayTile -> HH.HTML p i
tileToElem tile = HH.span [ HP.class_ (H.ClassName (tileClasses tile)) ] []

-- | Builds a row of HTML tags including the parent one
-- | for given `DisplayTile`s
tilesRowElem :: forall p i. Array DisplayTile -> HH.HTML p i
tilesRowElem tiles =
  HH.div
    [ HP.class_ (H.ClassName "level-row") ]
    (map tileToElem tiles)

buildTile :: Point -> Level -> DisplayTile
buildTile p { player: { pos, direction }, tiles } = withTile (lookup p tiles)

  where

  withTile tile
    | p == pos && tile == Just Water = Swimming direction
    | p == pos = Boy direction
    | otherwise = maybe Floor Tile tile
