module Rendering where

import Tile exposing (..)
import Grid

import Graphics.Element as Element exposing (..)
import Graphics.Collage as Draw
import Color exposing (..)
import Text exposing (..)


tileSize : Float
tileSize = 106.25


tileMargin : Float
tileMargin = 15

gridWidth =
  Grid.width tileSize tileMargin


tileColor : Tile -> Color
tileColor tile =
  case tile of
    Number 2 -> rgb 238 228 218
    Number 4 -> rgb 237 224 200
    Number 8 -> rgb 242 177 121
    Number 16 -> rgb 245 149 99
    Number 32 -> rgb 246 124 95
    Number 64 -> rgb 246 94 59
    Number 128 -> rgb 237 207 114
    Number 256 -> rgb 237 204 97
    Number 512 -> rgb 237 200 80
    Number 1024 -> rgb 237 197 63
    Number 2048 -> rgb 237 194 46
    _ -> rgba 238 228 218 0.35


tileTextColor : Tile -> Color
tileTextColor tile =
  case tile of
    Number n ->
      if n >= 8 then
        (rgb 249 246 242)
      else
        (rgb 119 110 101)
    _ -> black


tileTextSize : Tile -> Float
tileTextSize tile =
  case tile of
    Number 128 -> 45
    Number 256 -> 45
    Number 512 -> 45
    Number 1024 -> 35
    Number 2048 -> 35
    _ -> 55


tileTextStyle : Tile -> Style
tileTextStyle tile =
  { typeface = [ "Helvetica Neue", "Arial", "sans-serif" ]
  , height = Just (tileTextSize tile)
  , color = tileTextColor tile
  , bold = True
  , italic = False
  , line = Nothing
  }


displayTile : Tile -> Element
displayTile tile =
  let
    tileSize' = round tileSize
    tileBackground = Draw.square tileSize
      |> Draw.filled (tileColor tile)
    forms =
      case tile of
        Number n ->
          [ tileBackground
          , n
            |> toString
            |> Text.fromString
            |> Text.style (tileTextStyle tile)
            |> Element.centered
            |> Draw.toForm
          ]
        Empty ->
          [ tileBackground ]
  in
  Draw.collage tileSize' tileSize' forms


displayTileAtCoordinates : (Tile, Int, Int) -> Draw.Form
displayTileAtCoordinates (t,i,j) =
  let position =
    ( (tileSize + tileMargin) * (toFloat i - (toFloat Grid.size - 1)/2)
    , (-1) * (tileSize + tileMargin) * (toFloat j - (toFloat Grid.size - 1)/2)
    )
  in
  Draw.move position <| Draw.toForm <| displayTile t
