module Overlay where

import Grid

import Graphics.Collage as Draw
import Graphics.Element as Ele
import Color
import Text


-- MODEL

type Overlay
  = Message String
  | None


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

draw : Overlay -> Draw.Form
draw overlay =
  let
    backgroundColor =
      Color.rgba 237 194 46 0.5
    background =
      Draw.square Grid.width
        |> Draw.filled backgroundColor
    forms =
      case overlay of
        Message str ->
          [ background
          , str
            |> Text.fromString
            |> Text.style style
            >> Ele.centered
            >> Draw.toForm
          ]
        None ->
          []
  in
  Draw.group forms
