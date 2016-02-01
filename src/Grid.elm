module Grid where

import List.Extra as List2
import Graphics.Collage as Draw
import Color


-- MODEL

type alias AbstractGrid a =
  List (List a)


type alias Dimensions =
  { size : Int
  , tile :
    { width : Float
    , margin : Float
    }
  }

d : Dimensions
d =
  { size = 4
  , tile =
    { width = 106.25
    , margin = 15
    }
  }


size : Int
size =
  d.size


width : Float
width =
  (toFloat d.size) * d.tile.width + (1 + toFloat d.size) * d.tile.margin


offset : Int -> Float
offset x =
  (d.tile.width + d.tile.margin) * (toFloat x - (toFloat d.size - 1) / 2)


-- UPDATE

rotate : AbstractGrid a -> AbstractGrid a
rotate grid =
  List2.transpose grid
    |> List.map List.reverse


-- VIEW

draw : AbstractGrid a -> List Draw.Form -> Draw.Form
draw grid tileForms =
  (background (Color.rgb 187 173 160)) :: tileForms
    |> Draw.group


background : Color.Color -> Draw.Form
background color =
  Draw.square width
    |> Draw.filled color
