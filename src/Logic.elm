module Logic where

import List.Extra exposing (getAt)

import Input exposing (..)
import Game exposing (..)
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
slideRow r = let grouped =
  groupedByTwo <| List.filter (\t -> t /= Empty) r
    in (
        List.take Grid.size
        <| (List.map ( Tile.fromInt << List.sum << (List.map Tile.toInt)) grouped)
            ++ List.repeat Grid.size Empty
      , List.sum << (List.map Tile.toInt)
        <| List.concat
        <| List.filter (\x -> List.length x > 1) grouped
    )


slideGrid : Direction -> Grid -> (Grid, Int)
slideGrid dir grid =
  if (dir == None) then
    (grid,0)
  else
    let
      rotatedGrid = (case dir of
        Down -> Grid.rotate
        Right -> Grid.rotate >> Grid.rotate
        Up -> Grid.rotate >> Grid.rotate >> Grid.rotate
        otherwise -> identity)
        <| grid

      rowsWithScores = List.map slideRow rotatedGrid

      slidRotatedGrid = List.map (fst) rowsWithScores
      scoreGained = List.sum <| List.map (snd) rowsWithScores

      slidGrid = (case dir of
        Up -> Grid.rotate
        Right -> Grid.rotate >> Grid.rotate
        Down -> Grid.rotate >> Grid.rotate >> Grid.rotate
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
      up = fst <| slideGrid Up g
      down = fst <| slideGrid Down g
      left = fst <| slideGrid Left g
      right = fst <| slideGrid Right g
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


lose : Game.State -> Game.State
lose state =
  { state
  | progress = GameOver
  }


win : Game.State -> Game.State
win state =
  { state
  | progress = Won
  }


-- a list of the coordinates of the empty tiles in a grid
emptyTiles : Grid -> List (Int, Int)
emptyTiles g =
  List.map (\(_,i,j) -> (i,j))
    <| List.filter (\(t,_,_) -> t == Empty)
    <| Tile.withCoordinates g


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
        (newTile float2)
      }


newGame : Input -> Game.State
newGame input =
  let
    randoms = [0,1,2,3]
      |> List.map (getAt input.randomFloats)
      |> List.map (Maybe.withDefault 0)
    (i1,i2,i3,i4) = (0, 0.4, 0.6, 0.1)
  in
    placeRandomTile i1 i2
 <| placeRandomTile i3 i4
 <| Game.default


stepGame : Input -> Game.State -> Game.State
stepGame input state =
    if input.controls.newGame then
      newGame input
    else if state.progress /= InProgress then -- can probably go due to else
      state
    else if gameWon state.grid then
      win state
    else if gameLost state.grid then
      lose state
    else if input.controls.push /= None then
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
