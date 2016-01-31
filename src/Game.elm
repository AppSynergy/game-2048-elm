module Game where

import Tile exposing (Tile,Grid)
import Grid

import List.Extra exposing (transpose)


-- MODEL

type Progress
  = InProgress
  | GameOver
  | Won


type alias State =
  { grid: Grid
  , score: Int
  , gameProgress: Progress
  }


default : State
default =
  { grid = Tile.emptyGrid
  , score = 0
  , gameProgress = InProgress
  }

-- UPDATE
