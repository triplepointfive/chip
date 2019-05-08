module Display (DisplayTile(..), levelTiles, tilesRowElem) where

import Level
import Utils

import Data.Array (range)
import Data.Map (lookup)
import Data.Maybe (maybe)
import Data.Tuple (Tuple(..))
import Prelude

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Properties as HP

data DisplayTile = Floor | Tile Tile | Boy Direction

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

tileClasses :: DisplayTile -> Array String
tileClasses Floor = ["tile", "-floor"]
tileClasses (Tile Wall) = ["tile", "-wall"]
tileClasses (Tile Chip) = ["tile", "-chip"]
tileClasses (Tile (Key Red)) = ["tile", "-key", "-red"]
tileClasses (Tile (Key Cyan)) = ["tile", "-key", "-cyan"]
tileClasses (Tile (Key Yellow)) = ["tile", "-key", "-yellow"]
tileClasses (Tile (Key Green)) = ["tile", "-key", "-green"]
tileClasses (Tile (Door Red)) = ["tile", "-door", "-red"]
tileClasses (Tile (Door Cyan)) = ["tile", "-door", "-cyan"]
tileClasses (Tile (Door Yellow)) = ["tile", "-door", "-yellow"]
tileClasses (Tile (Door Green)) = ["tile", "-door", "-green"]
tileClasses (Boy Down) = ["tile", "-boy", "-down"]
tileClasses (Boy Left) = ["tile", "-boy", "-left"]
tileClasses (Boy Up) = ["tile", "-boy", "-up"]
tileClasses (Boy Right) = ["tile", "-boy", "-right"]

tileToElem :: forall p i. DisplayTile -> HH.HTML p i
tileToElem tile = HH.span [ HP.classes $ map H.ClassName (tileClasses tile) ] []

tilesRowElem :: forall p i. Array DisplayTile -> HH.HTML p i
tilesRowElem tiles =
  HH.div
    [ HP.class_ (H.ClassName "level-row") ]
    (map tileToElem tiles)

buildTile :: Point -> Level -> DisplayTile
buildTile p { player: { pos }, tiles }
  | p == pos  = Boy Down
  | otherwise = maybe Floor Tile (lookup p tiles)
