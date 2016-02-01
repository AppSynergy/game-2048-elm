module Randomize where

import Random

-- MODEL

generator : Random.Generator (List Float)
generator =
    Random.list 4 (Random.float 0 1)


type alias Model =
  { seed : Random.Seed
  , generator : Random.Generator (List Float)
  , value : List Float
  }


init : Model
init =
  { seed = Random.initialSeed 1
  , generator = generator
  , value = []
  }


-- UPDATE

update : a -> Model -> Model
update a model =
  let
    (value,seed) = Random.generate model.generator model.seed
  in
  { model
  | value = value
  , seed = seed
  }


-- VIEW

view : Model -> List Float
view model =
  model.value


-- SIGNALS

signal : Signal a -> Signal (List Float)
signal trigger =
  Signal.foldp update init trigger
    |> Signal.map view
