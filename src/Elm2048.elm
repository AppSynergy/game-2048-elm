module Elm2048 where

import InputModel exposing (Input, Controls, playerDirection, randomFloats)
import GameModel exposing (defaultGame, GameState)
import Logic exposing (stepGame)
import Rendering exposing (display)


-- Outgoing Ports

port score : Signal Int
port score =
  Signal.map (\x -> x.score) gameState


-- Incoming Ports

port newGameButton : Signal Bool


-- Signals

controls =
  Signal.map2 (\a b -> { newGameButtonPressed=b, tilePushDirection=a }) playerDirection newGameButton


input =
  let
    i = (\a b -> { controls=a, randomFloats=b })
  in
  Signal.map2 i controls (randomFloats controls)


gameState : Signal GameState
gameState =
  Signal.foldp stepGame defaultGame input


main =
  Signal.map display gameState
