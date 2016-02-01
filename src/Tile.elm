module Tile where

import Grid
import Tile.Style

import List.Extra exposing (getAt)
import Graphics.Collage as Draw
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
      |> Draw.filled (Tile.Style.color (toInt tile))
    forms =
      case tile of
        Number n ->
          [ tileBackground
          , toString n
            |> Utils.textForm (Tile.Style.text (toInt tile))
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
