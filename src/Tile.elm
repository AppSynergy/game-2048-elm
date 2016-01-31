module Tile where

import List.Extra exposing (getAt)

import Grid


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


newTile : Float -> Tile
newTile x =
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

-- based on a float that will be random
-- return Just the coordinates of an empty tile in a
-- grid if one exists, or Nothing if there are none
newTileIndex : Float -> Grid -> Maybe (Int, Int)
newTileIndex x g =
    let
      emptyTileIndices = emptyTiles g
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


set : (Int, Int) -> Grid -> Tile -> Grid
set (i, j) g t =
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
    xcoords = List.concat (List.repeat Grid.size range)
    ycoords = [0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3]
  in
  List.map3 (,,) flat xcoords ycoords
