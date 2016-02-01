module Input where

import Keyboard
import Random


-- MODEL

type Direction = Up | Down | Left | Right | None


type alias Controls =
  { push: Direction
  , newGame: Bool
  }


type alias Input =
  { controls: Controls
  , randomFloats: List Float
  }


-- SIGNALS

move : Signal Direction
move =
  let toDirection ds  =
    if ds == { x=0, y=1 } then Up
    else if ds == { x=0, y=-1 } then Down
    else if ds == { x=1, y=0 } then Right
    else if ds == { x=-1, y=0 } then Left
    else None
  in
  Signal.merge
    (Signal.map toDirection Keyboard.arrows)
    (Signal.map toDirection Keyboard.wasd)


-- provides four random floats that will be used for random events.
-- changes every time signal s changes
randomizer : Signal a -> Signal (List Float)
--randomFloats s = floatList <| Signal.sampleOn s <| Signal.constant 4
randomizer s = Signal.constant [0.65, 0.78, 0.72, 0.13, 0.82]

floatList : Random.Generator (List Float)
floatList =
    Random.list 10 (Random.float 0 1)
