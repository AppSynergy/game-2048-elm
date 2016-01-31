module Game where

import Rendering exposing (..)
import Tile exposing (Tile, Grid)
import Grid
import Overlay

import Graphics.Element as Element


-- MODEL

type Progress
  = InProgress
  | GameOver
  | Won


type alias State =
  { grid: Grid
  , score: Int
  , progress: Progress
  }


default : State
default =
  { grid = Tile.emptyGrid
  , score = 0
  , progress = InProgress
  }

-- UPDATE


-- VIEW

view : State -> Element.Element
view state =
  let overlayer =
    case state.progress of
      GameOver ->
        applyOverlay Overlay.viewGameOver
      Won ->
        applyOverlay Overlay.viewWon
      _ ->
        identity
  in
  overlayer (Grid.draw state.grid (drawTiles state.grid) gridWidth)
