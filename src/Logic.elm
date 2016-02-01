module Logic where

import List.Extra exposing (getAt)

import Input exposing (Input)
import Grid
import Tile exposing (Tile, Grid)

-- takes a list of values and 'slides' them to the left,
-- joining in lists pairs of adjacent identical values.
groupedByTwo : List a -> List (List a)
groupedByTwo l =
  case l of
    [x] ->
      [[x]]
    [x,y] ->
      if (x == y) then
        [[x,y]]
      else
        [[x],[y]]
    (x::y::xs) ->
      if (x == y) then
        ([x,y] :: (groupedByTwo xs))
      else
        ([x] :: (groupedByTwo (y::xs)))
    _ -> []


-- slides list of tiles to left, merging tiles where necessary
-- returning a full list of four tiles, and points gained
slideRow : List Tile -> (List Tile, Int)
slideRow row =
  let
    emptyRow =
      List.repeat Grid.size Tile.Empty
    grouped =
      row
        |> List.filter (\x -> x /= Tile.Empty)
        |> groupedByTwo
  in
  ( List.take Grid.size
    <| (List.map ( Tile.fromInt << List.sum << (List.map Tile.toInt)) grouped)
      ++ emptyRow
  , grouped
    |> List.filter (\x -> List.length x > 1)
    >> List.concat
    |> List.map (Tile.toInt)
    >> List.sum
  )


slideGrid : Input.Direction -> Grid -> (Grid, Int)
slideGrid dir grid =
  if (dir == Input.None) then
    (grid,0)
  else
    let
      rotatedGrid = (case dir of
        Input.Down -> Grid.rotate
        Input.Right -> Grid.rotate >> Grid.rotate
        Input.Up -> Grid.rotate >> Grid.rotate >> Grid.rotate
        otherwise -> identity)
        <| grid

      rowsWithScores = List.map slideRow rotatedGrid

      slidRotatedGrid = List.map (fst) rowsWithScores
      scoreGained = List.sum <| List.map (snd) rowsWithScores

      slidGrid = (case dir of
        Input.Up -> Grid.rotate
        Input.Right -> Grid.rotate >> Grid.rotate
        Input.Down -> Grid.rotate >> Grid.rotate >> Grid.rotate
        otherwise -> identity)
        <| slidRotatedGrid

    in (slidGrid, scoreGained)




-- If you can't slide, you've lost!
gameLost : Grid -> Bool
gameLost g =
    let
      up = fst <| slideGrid Input.Up g
      down = fst <| slideGrid Input.Down g
      left = fst <| slideGrid Input.Left g
      right = fst <| slideGrid Input.Right g
    in
    List.foldl (\x y -> x && y) True
      [ g /= Tile.emptyGrid
      , up == down
      , down == left
      , left == right
      , right == g
      ]


-- If you've made 2048, you've won!
gameWon : Grid -> Bool
gameWon g =
  0 /= (List.length <| List.filter (\t -> t == Tile.Number 2048) <| List.concat g)
