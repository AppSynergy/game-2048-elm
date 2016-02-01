module Utils where

import Graphics.Collage as Draw
import Graphics.Element as Ele
import Text


textForm : Text.Style -> String -> Draw.Form
textForm style string =
  string
    |> Text.fromString
    |> Text.style style
    |> Ele.centered
    |> Draw.toForm
