module Game where

import Grid
import Tile exposing (Tile, Grid,displayTileAtCoordinates)
import Grid
import Overlay

import Graphics.Element as Ele
import Graphics.Collage as Draw

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

lose : State -> State
lose state =
  { state
  | progress = GameOver
  }


win : State -> State
win state =
  { state
  | progress = Won
  }


-- VIEW

view : State -> Ele.Element
view state =
  let overlayer =
    case state.progress of
      GameOver ->
        applyOverlay (Overlay.view "Game Over")
      Won ->
        applyOverlay (Overlay.view "You Won!")
      _ ->
        identity
  in
  overlayer (Grid.draw state.grid (drawTiles state.grid) Grid.width)


applyOverlay : Ele.Element -> Ele.Element -> Ele.Element
applyOverlay overlay grid =
  Draw.collage (round Grid.width) (round Grid.width)
    [ Draw.toForm grid
    , Draw.toForm overlay
    ]


drawTiles : Grid -> List Draw.Form
drawTiles grid =
  Tile.withCoordinates grid
    |> List.map Tile.displayTileAtCoordinates
