module Input where

import Keyboard


-- MODEL

type Direction
  = Up
  | Down
  | Left
  | Right
  | None


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
