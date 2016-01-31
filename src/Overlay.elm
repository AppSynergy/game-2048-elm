module Overlay where

import Rendering exposing (gridWidth)
import Tile

import Graphics.Collage as Draw
import Graphics.Element as Ele
import Color
import Text


-- MODEL

style : Text.Style
style =
  { typeface = [ "Helvetica Neue", "Arial", "sans-serif" ]
  , height = Just 55.0
  , color = Color.white
  , bold = True
  , italic = False
  , line = Nothing
  }


-- VIEW

view : String -> Ele.Element
view string =
  let
    backgroundColor = Color.rgba 237 194 46 0.5
  in
  Draw.collage (round gridWidth) (round gridWidth)
    [ Draw.square gridWidth
      |> Draw.filled backgroundColor
    , string
      |> Text.fromString
      |> Text.style style
      >> Ele.centered
      >> Draw.toForm
    ]
