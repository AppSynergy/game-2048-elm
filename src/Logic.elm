module Logic where

import List.Extra exposing (getAt)

import Input exposing (Input)
import Game exposing (win, lose, Progress)
import Grid
import Tile exposing (..)

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
      List.repeat Grid.size Empty
    grouped =
      row
        |> List.filter (\x -> x /= Empty)
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


slideGameState : Input -> Game.State -> Game.State
slideGameState input state =
  let
    newGridScore = slideGrid input.controls.push state.grid
  in
  if (fst newGridScore == state.grid) then
    state
  else
    { state
    | grid = fst newGridScore
    , score = state.score + snd newGridScore
    }


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
      [ g /= emptyGrid
      , up == down
      , down == left
      , left == right
      , right == g
      ]


-- If you've made 2048, you've won!
gameWon : Grid -> Bool
gameWon g =
  0 /= (List.length <| List.filter (\t -> t == Number 2048) <| List.concat g)



placeRandomTile : Float -> Float -> Game.State -> Game.State
placeRandomTile float1 float2 state =
    let
      tileIndex = newTileIndex float1 state.grid
    in
    if (tileIndex == Nothing) then
      state
    else
      { state
      | grid = Tile.set
        (Maybe.withDefault (0,0) tileIndex)
        state.grid
        (Tile.init float2)
      }


newGame : Input -> Game.State
newGame input =
  let
    randoms = [0,1,2,3]
      |> List.map (getAt input.randomFloats)
      |> List.map (Maybe.withDefault 0)
    d1 = Debug.log "randoms" randoms
    (i1,i2,i3,i4) = (0.87, 0.4, 0.6, 0.1)
    --(i1,i2,i3,i4) = List.foldr (,,,) randoms
  in
    placeRandomTile i1 i2
 <| placeRandomTile i3 i4
 <| Game.default


stepGame : Input -> Game.State -> Game.State
stepGame input state =
    if input.controls.newGame then
      newGame input
    else if state.progress /= Game.InProgress then -- can probably go due to else
      state
    else if gameWon state.grid then
      win state
    else if gameLost state.grid then
      lose state
    else if input.controls.push /= Input.None then
      let
        pushedState = slideGameState input state
      in
      if (pushedState == state) then
        state
      else
        placeRandomTile
          (Maybe.withDefault 0 (getAt input.randomFloats 0))
          (Maybe.withDefault 0 (getAt input.randomFloats 1))
          pushedState
    else
      state
