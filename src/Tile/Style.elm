module Tile.Style where

import Color
import Text


-- MODEL

text : Int -> Text.Style
text n =
  { typeface = [ "Helvetica Neue", "Arial", "sans-serif" ]
  , height = textSize n
  , color = textColor n
  , bold = True
  , italic = False
  , line = Nothing
  }


color : Int -> Color.Color
color n =
  case n of
    2 -> Color.rgb 238 228 218
    4 -> Color.rgb 237 224 200
    8 -> Color.rgb 242 177 121
    16 -> Color.rgb 245 149 99
    32 -> Color.rgb 246 124 95
    64 -> Color.rgb 246 94 59
    128 -> Color.rgb 237 207 114
    256 -> Color.rgb 237 204 97
    512 -> Color.rgb 237 200 80
    1024 -> Color.rgb 237 197 63
    2048 -> Color.rgb 237 194 46
    _ -> Color.rgba 238 228 218 0.35


textColor : Int -> Color.Color
textColor n =
  if n >= 8 then
    (Color.rgb 249 246 242)
  else
    (Color.rgb 119 110 101)


textSize : Int -> Maybe Float
textSize n =
  if n >= 1024 then Just 35
  else if n >= 128 then Just 45
  else Just 55
