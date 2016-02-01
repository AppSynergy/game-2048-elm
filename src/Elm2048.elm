module Elm2048 where

import Graphics.Element as Ele

import Input exposing (Input)
import Game
import Randomize
import Logic exposing (stepGame)


-- PORTS

port score : Signal Int
port score =
  Signal.map (\x -> x.score) gameState


port start : Signal Bool


-- SIGNALS

input : Signal Input
input =
  let
    controls =
      Signal.map2 (\a b -> { newGame=b, push=a }) Input.move start
    i =
      (\a b -> { controls=a, randomFloats=b })
  in
  Signal.map2 i controls (Randomize.signal controls)


gameState : Signal Game.State
gameState =
  Signal.foldp stepGame Game.default input


main : Signal Ele.Element
main =
  Signal.map Game.view gameState
