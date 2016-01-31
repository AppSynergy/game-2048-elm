{--
Copyright (c) 2014 Josh Kirklin
This source is subject to the MIT License.
Please see the LICENSE file for more information.
All other rights reserved.
--}

module InputModel where

import Keyboard
import Random


-- the direction to shift the grid
type Direction = Up | Down | Left | Right | None


type alias Controls =
  { tilePushDirection: Direction
  , newGameButtonPressed: Bool
  }


-- define the inputs that the game will depend upon
type alias Input =
  { controls: Controls
  , randomFloats: List Float
  }


-- make a signal that is the direction that the user has chosen.
playerDirection : Signal Direction
playerDirection =
  let toDirection ds  =
    if ds == { x=0, y=1 } then Up
    else if ds == { x=0, y=-1 } then Down
    else if ds == { x=1, y=0 } then Right
    else if ds == { x=-1, y=0 } then Left
    else None
  in
  -- with both the wasd and arrow keys
  Signal.merge
    (Signal.map toDirection Keyboard.arrows)
    (Signal.map toDirection Keyboard.wasd)


-- provides four random floats that will be used for random events.
-- changes every time signal s changes
randomFloats : Signal a -> Signal (List Float)
--randomFloats s = floatList <| Signal.sampleOn s <| Signal.constant 4
randomFloats s = Signal.constant [0.65, 0.78, 0.72, 0.13, 0.82]

floatList : Random.Generator (List Float)
floatList =
    Random.list 10 (Random.float 0 1)
