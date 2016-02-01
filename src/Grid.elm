module Grid where

import List.Extra as List2
import Graphics.Collage as Draw
import Graphics.Element as Ele
import Color

-- MODEL

size : Int
size = 4


tileSize : Float
tileSize = 106.25


tileMargin : Float
tileMargin = 15


width : Float
width =
  (toFloat size) * tileSize + (1 + toFloat size) * tileMargin


type alias AbstractGrid a =
  List (List a)


-- UPDATE

rotate : AbstractGrid a -> AbstractGrid a
rotate grid =
  List2.transpose grid
    |> List.map List.reverse


-- VIEW

draw : AbstractGrid a -> List Draw.Form -> Draw.Form
draw grid tileForms =
  let
    background = Draw.square width
      |> Draw.filled (Color.rgb 187 173 160)
  in
  background :: tileForms
    |> Draw.group
