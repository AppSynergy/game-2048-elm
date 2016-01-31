module Overlay where

import Rendering exposing (gridWidth, tileTextStyle)
import Tile

import Graphics.Collage as Draw
import Graphics.Element as Ele
import Color exposing (Color)
import Text exposing (Style)


gameOverOverlayStyle : Style
gameOverOverlayStyle =
  tileTextStyle <| Tile.Number 2


wonOverlayStyle : Style
wonOverlayStyle =
  tileTextStyle <| Tile.Number 16


themeColors =
  { overlayText = Color.rgba 237 194 46 0.5 }


viewGameOver : Ele.Element
viewGameOver =
  "Game over!"
    |> view gameOverOverlayStyle themeColors.overlayText


viewWon : Ele.Element
viewWon =
  "You won!"
    |> view wonOverlayStyle themeColors.overlayText


view : Style -> Color -> String ->  Ele.Element
view style color string =
  Draw.collage (round gridWidth) (round gridWidth)
    [
      Draw.square gridWidth
        |> Draw.filled color
    , Draw.toForm (Ele.show string)
    ]
