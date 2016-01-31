module GameModel where

import Tile exposing (Tile,Grid)
import Grid

import List.Extra exposing (transpose, getAt)


(!) = getAt
infixl 9 !



type Progress =
  InProgress | GameOver | Won


type alias GameState =
  { grid: Grid
  , score: Int
  , gameProgress: Progress
  }




defaultGame : GameState
defaultGame =
  { grid = Tile.emptyGrid
  , score = 0
  , gameProgress = InProgress
  }





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



tilesWithCoordinates : Grid -> List (Tile,Int,Int)
tilesWithCoordinates grid =
  let
    range = [0..(Grid.size-1)]
    flat = List.concat grid
    xcoords = List.concat (List.repeat Grid.size range)
    ycoords = [0,0,0,0,1,1,1,1,2,2,2,2,3,3,3,3]
  in
  List.map3 (,,) flat xcoords ycoords


rotateGrid : Grid -> Grid
rotateGrid g = List.map List.reverse <| transpose g
