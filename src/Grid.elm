module Grid where

import List.Extra exposing (transpose)

size : Int
size = 4


type alias Gridlike a =
  List (List a)


rotate : Gridlike a -> Gridlike a
rotate g = List.map List.reverse <| transpose g
