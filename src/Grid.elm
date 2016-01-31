module Grid where

import List.Extra exposing (transpose)


size : Int
size = 4


-- This module does not care what `a` is.
type alias AbstractGrid a =
  List (List a)


rotate : AbstractGrid a -> AbstractGrid a
rotate grid =
  transpose grid
    |> List.map List.reverse
