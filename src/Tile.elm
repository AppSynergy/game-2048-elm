module Tile where

import Grid

import List.Extra exposing (getAt)
import Graphics.Element as Ele
import Graphics.Collage as Draw
import Color
import Text exposing (Style)
import Utils

-- MODEL

type Tile
  = Number Int
  | Empty


type alias Grid
  = Grid.AbstractGrid Tile


emptyGrid : Grid
emptyGrid =
  Empty
    |> List.repeat Grid.size
    |> List.repeat Grid.size


init : Float -> Tile
init x =
  if (x < 0.9) then
    (Number 2)
  else
    (Number 4)


-- UTILS

toInt : Tile -> Int
toInt t = case t of
    Number n -> n
    _ -> 0


fromInt : Int -> Tile
fromInt n = case n of
    0 -> Empty
    _ -> Number n


-- UPDATE

add : Float -> Float -> Grid -> Grid
add float1 float2 grid =
    let
      tileIndex = findEmpty float1 grid
    in
    if (tileIndex == Nothing) then
      grid
    else
      set
        (Maybe.withDefault (0,0) tileIndex)
        (init float2)
        grid


-- based on a float that will be random
-- return Just the coordinates of an empty tile in a
-- grid if one exists, or Nothing if there are none
findEmpty : Float -> Grid -> Maybe (Int, Int)
findEmpty x grid =
    let
      emptyTileIndices = emptyTiles grid
    in
    case emptyTileIndices of
      [] ->
        Nothing
      _ ->
        Just
          (Maybe.withDefault (0,0) (getAt emptyTileIndices
           (floor <| (toFloat <| List.length emptyTileIndices) * x)))


-- a list of the coordinates of the empty tiles in a grid
emptyTiles : Grid -> List (Int, Int)
emptyTiles g =
  List.map (\(_,i,j) -> (i,j))
    <| List.filter (\(t,_,_) -> t == Empty)
    <| withCoordinates g


set : (Int, Int) -> Tile -> Grid -> Grid
set (i, j) t g =
    let
      r = getAt g j -- jth row
      nr = case r of
        Just a ->
          (List.take i (a)) ++ [t] ++ (List.drop (i+1) a)
        Nothing ->
          []
        -- ith element changed in jth row
    in
    (List.take j g) ++ [nr] ++ (List.drop (j+1) g)
      -- new grid with modified jth row


withCoordinates : Grid -> List (Tile,Int,Int)
withCoordinates grid =
  let
    range = [0..(Grid.size-1)]
    flat = List.concat grid
    repeat x = List.repeat Grid.size x
    xcoords = repeat range
      |> List.concat
    ycoords = List.map (\y -> repeat y) range
      |> List.concat
  in
  List.map3 (,,) flat xcoords ycoords


-- VIEW

displayTile : Tile -> Draw.Form
displayTile tile =
  let
    tileSize' = round Grid.tileSize
    tileBackground = Draw.square Grid.tileSize
      |> Draw.filled (tileColor tile)
    forms =
      case tile of
        Number n ->
          [ tileBackground
          , toString n
            |> Utils.textForm (tileTextStyle tile)
          ]
        Empty ->
          [ tileBackground ]
  in
  Draw.group forms


displayTileAtCoordinates : (Tile, Int, Int) -> Draw.Form
displayTileAtCoordinates (t,i,j) =
  let position =
    ( (Grid.tileSize + Grid.tileMargin) * (toFloat i - (toFloat Grid.size - 1)/2)
    , (-1) * (Grid.tileSize + Grid.tileMargin) * (toFloat j - (toFloat Grid.size - 1)/2)
    )
  in
  Draw.move position <| displayTile t



tileColor : Tile -> Color.Color
tileColor tile =
  case tile of
    Number 2 -> Color.rgb 238 228 218
    Number 4 -> Color.rgb 237 224 200
    Number 8 -> Color.rgb 242 177 121
    Number 16 -> Color.rgb 245 149 99
    Number 32 -> Color.rgb 246 124 95
    Number 64 -> Color.rgb 246 94 59
    Number 128 -> Color.rgb 237 207 114
    Number 256 -> Color.rgb 237 204 97
    Number 512 -> Color.rgb 237 200 80
    Number 1024 -> Color.rgb 237 197 63
    Number 2048 -> Color.rgb 237 194 46
    _ -> Color.rgba 238 228 218 0.35


tileTextColor : Tile -> Color.Color
tileTextColor tile =
  case tile of
    Number n ->
      if n >= 8 then
        (Color.rgb 249 246 242)
      else
        (Color.rgb 119 110 101)
    _ -> Color.black


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
