module Tile where

import List.Extra exposing (getAt)

import Grid

type Tile =
  Number Int | Empty


type alias Grid =
  List (List Tile)


emptyGrid : Grid
emptyGrid =
  Empty
    |> List.repeat Grid.size
    |> List.repeat Grid.size


toInt : Tile -> Int
toInt t = case t of
    Number n -> n
    _ -> 0


fromInt : Int -> Tile
fromInt n = case n of
    0 -> Empty
    _ -> Number n
