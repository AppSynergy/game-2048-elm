module Elm2048 where

import Graphics.Element as Ele

import InputModel exposing (Input, Controls, playerDirection, randomFloats)
import GameModel exposing (defaultGame, GameState)
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
      Signal.map2 (\a b -> { newGameButtonPressed=b, tilePushDirection=a }) playerDirection newGameButton
    i = (\a b -> { controls=a, randomFloats=b })
  in
  Signal.map2 i controls (randomFloats controls)


gameState : Signal GameState
gameState =
  Signal.foldp stepGame defaultGame input


main : Signal Ele.Element
main =
  Signal.map Rendering.display gameState
