module Input where

import Keyboard
import Random
import Random.Signal
import Random.Float
import Random.Extra
import Time

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
randomizer s =
  let
    --d = Debug.log "gen" prob
    g = generateEvery s floatList
  in
  g
  --Signal.constant [0.35, 0.78, 0.72, 0.13, 0.82]


prob =
  Random.Extra.generateN 4 probGen (Random.initialSeed 443)

--generateN : Int -> Generator a -> Seed -> List a
--Generate n values from a generator.

probGen : Random.Generator Float
probGen =
  Random.Float.probability


floatList : Random.Generator (List Float)
floatList =
    Random.list 4 (Random.float 0 1)



type alias Model a =
  { seed : Random.Seed
  , generator : Random.Generator a
  , value : Float
  }

generateEvery : Signal a -> Random.Generator a -> Signal b
generateEvery s generator =
  let
    initialModel : Model a
    initialModel =
      { seed = Random.initialSeed 1
      , generator = generator
      , value = 0
      }

    update : Random.Seed -> Model a -> Model a
    update seed model =
      { model | seed = seed }

    view : Model a -> a
    view model =
      fst <| Random.generate model.generator model.seed
  in
    Signal.map view (Signal.foldp update initialModel (Random.Signal.randomSeedEvery 666))
