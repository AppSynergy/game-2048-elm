module Grid where

import List.Extra as List2
import Graphics.Collage as Draw
import Graphics.Element as Ele
import Color

-- MODEL

size : Int
size = 4


width : Float -> Float -> Float
width aSize aMargin =
  (toFloat size) * aSize + (1 + toFloat size) * aMargin


type alias AbstractGrid a =
  List (List a)


-- UPDATE

rotate : AbstractGrid a -> AbstractGrid a
rotate grid =
  List2.transpose grid
    |> List.map List.reverse


-- VIEW

draw : AbstractGrid a -> List Draw.Form -> Float -> Ele.Element
draw grid aDraw aWidth =
  let
    background = Draw.square aWidth
      |> Draw.filled (Color.rgb 187 173 160)
  in
  Draw.collage (round aWidth) (round aWidth) ([background] ++ aDraw)
