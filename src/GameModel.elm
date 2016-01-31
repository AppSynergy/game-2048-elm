{--
Copyright (c) 2014 Josh Kirklin
This source is subject to the MIT License.
Please see the LICENSE file for more information.
All other rights reserved.
--}

module GameModel where


import List.Extra exposing (transpose, getAt)


(!) = getAt
infixl 9 !


type Tile =
  Number Int | Empty


type alias Grid =
  List (List Tile)


type Progress =
  InProgress | GameOver | Won


type alias GameState =
  { grid: Grid
  , score: Int
  , gameProgress: Progress
  }


gridSize : Int
gridSize = 4


defaultGame : GameState
defaultGame =
  { grid = emptyGrid
  , score = 0
  , gameProgress = InProgress
  }


emptyGrid : Grid
emptyGrid =
  List.repeat gridSize (List.repeat gridSize Empty)


readTile : (Int, Int) -> Grid -> Tile
readTile (i, j) grid =
  let
    row = Maybe.withDefault [] (getAt grid j)
  in
  Maybe.withDefault Empty (getAt row i)


-- change (?, set?) the tile at (i,j) in a grid
setTile : (Int, Int) -> Grid -> Tile -> Grid
setTile (i, j) g t =
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


tileToInt : Tile -> Int -- convert a tile to the int it represents
tileToInt t = case t of
    Number n -> n
    _ -> 0


intToTile : Int -> Tile -- convert an int to a tile representing it
intToTile n = case n of
    0 -> Empty
    _ -> Number n


tilesWithCoordinates : Grid -> List (Tile,Int,Int)
tilesWithCoordinates grid =
  let
    range = [0..(gridSize-1)]
    flat = List.concat grid
    xcoords = List.concat (List.repeat gridSize range)
    ycoords = [0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3]
  in
  List.map3 (,,) flat xcoords ycoords


rotateGrid : Grid -> Grid
rotateGrid g = List.map List.reverse <| transpose g
