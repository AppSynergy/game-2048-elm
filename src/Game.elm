module Game where

import Grid
import Tile exposing (Tile, Grid,displayTileAtCoordinates)
import Grid
import Overlay
import Logic
import Input exposing (Input, fetchRandom)

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


-- UPDATE


update : Input -> State -> State
update input state =
    if input.controls.newGame then
      newGame input
    else if state.progress /= InProgress then
      state
    else if Logic.gameWon state.grid then
      { state | progress = Won }
    else if Logic.gameLost state.grid then
      { state | progress = GameOver }
    else if input.controls.push /= Input.None then
      let
        pushedState = slideGameState input state
      in
      if (pushedState == state) then
        state
      else
        placeRandomTile
          (fetchRandom input 0)
          (fetchRandom input 1)
          pushedState
    else
      state


slideGameState : Input -> State -> State
slideGameState input state =
  let
    newGridScore = Logic.slideGrid input.controls.push state.grid
  in
  if (fst newGridScore == state.grid) then
    state
  else
    { state
    | grid = fst newGridScore
    , score = state.score + snd newGridScore
    }


newGame : Input -> State
newGame input =
  init
    |> placeRandomTile (fetchRandom input 0) (fetchRandom input 1)
    |> placeRandomTile (fetchRandom input 2) (fetchRandom input 3)



placeRandomTile : Float -> Float -> State -> State
placeRandomTile float1 float2 state =
    let
      tileIndex = Tile.newTileIndex float1 state.grid
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
