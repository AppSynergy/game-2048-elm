module GameModel where

import Tile exposing (Tile,Grid)
import Grid

import List.Extra exposing (transpose)


type Progress
  = InProgress
  | GameOver
  | Won


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
