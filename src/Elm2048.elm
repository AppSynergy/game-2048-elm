module Elm2048 where

import Graphics.Element as Ele

import Input exposing (Input, Controls, playerDirection, randomFloats)
import Game
import Logic exposing (stepGame)
import Rendering


-- PORTS

port score : Signal Int
port score =
  Signal.map (\x -> x.score) gameState


port newGameButton : Signal Bool


-- SIGNALS

input : Signal Input
input =
  let
    controls =
      Signal.map2 (\a b -> { newGame=b, push=a }) playerDirection newGameButton
    i = (\a b -> { controls=a, randomFloats=b })
  in
  Signal.map2 i controls (randomFloats controls)


gameState : Signal Game.State
gameState =
  Signal.foldp stepGame Game.default input


main : Signal Ele.Element
main =
  Signal.map Game.view gameState
