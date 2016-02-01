module Game where

import Tile exposing (Tile, Grid)
import Grid
import Overlay exposing (Overlay)
import Logic
import Input exposing (Input)

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


init : State
init =
  { grid = Tile.emptyGrid
  , score = 0
  , progress = InProgress
  }


init' : Input -> State
init' input =
  { init
  | grid = Tile.emptyGrid
    |> Tile.add (Input.random input 0) (Input.random input 1)
    |> Tile.add (Input.random input 2) (Input.random input 3)
  }


-- UPDATE

update : Input -> State -> State
update input state =
    if input.controls.newGame then
      init' input
    else if state.progress /= InProgress then
      state
    else if Logic.gameWon state.grid then
      { state | progress = Won }
    else if Logic.gameLost state.grid then
      { state | progress = GameOver }
    else if input.controls.push /= Input.None then
      let
        (grid,score) = Logic.slideGrid input.controls.push state.grid
      in
      if grid == state.grid then
        state
      else
        { state
        | grid = Tile.add
          (Input.random input 0)
          (Input.random input 1)
          grid
        , score = state.score + score
        }
    else
      state


-- VIEW

view : State -> Ele.Element
view state =
  let
    drawOverlay =
      case state.progress of
        GameOver ->
          Overlay.draw (Overlay.Message "Game Over")
        Won ->
          Overlay.draw (Overlay.Message "You Won!")
        _ ->
          Overlay.draw Overlay.None
    drawGrid =
      Grid.draw state.grid (drawTiles state.grid)
  in
  [drawGrid, drawOverlay]
    |> Draw.collage (round Grid.width) (round Grid.width)


drawTiles : Grid -> List Draw.Form
drawTiles grid =
  Tile.withCoordinates grid
    |> List.map Tile.draw
