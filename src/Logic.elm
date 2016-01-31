module Logic where

import List.Extra exposing (getAt)

import InputModel exposing (..)

import GameModel exposing (..)


(!) = getAt

-- takes a list of values and 'slides' them to the left,
-- joining in lists pairs of adjacent identical values.
groupedByTwo : List a -> List (List a)
groupedByTwo l = case l of
    [x] -> [[x]]
    [x,y] -> if (x == y) then [[x,y]] else [[x],[y]]
    (x::y::xs) -> if (x == y) then ([x,y] :: (groupedByTwo xs))
                    else ([x] :: (groupedByTwo (y::xs)))
    _ -> []


-- slides list of tiles to left, merging tiles where necessary
-- returning a full list of four tiles, and points gained
slideRow : List Tile -> (List Tile, Int)
slideRow r = let grouped =
  groupedByTwo <| List.filter (\t -> t /= Empty) r
    in (
        List.take gridSize
        <| (List.map ( intToTile << List.sum << (List.map tileToInt)) grouped)
            ++ List.repeat gridSize Empty
      , List.sum << (List.map tileToInt)
        <| List.concat
        <| List.filter (\x -> List.length x > 1) grouped
    )


slideGrid : Direction -> Grid -> (Grid, Int)
slideGrid dir grid =
  if (dir == None) then
    (grid,0)
  else
    let
      rotatedGrid = (case dir of
        Down  -> rotateGrid
        Right -> rotateGrid >> rotateGrid
        Up    -> rotateGrid >> rotateGrid >> rotateGrid
        otherwise -> identity)
        <| grid

      rowsWithScores = List.map slideRow rotatedGrid

      slidRotatedGrid = List.map (fst) rowsWithScores
      scoreGained = List.sum <| List.map (snd) rowsWithScores

      slidGrid = (case dir of
        Up  -> rotateGrid
        Right -> rotateGrid >> rotateGrid
        Down    -> rotateGrid >> rotateGrid >> rotateGrid
        otherwise -> identity)
        <| slidRotatedGrid

    in (slidGrid, scoreGained)

slideGameState : Input -> GameState -> GameState
slideGameState input gameState =
   let newGridScore = slideGrid input.controls.tilePushDirection gameState.grid
    in if (fst newGridScore == gameState.grid) then gameState else
        { gameState
        | grid = fst newGridScore
        , score = gameState.score + snd newGridScore
        }


-- check if none of the rows or columns of a grid can be slid in any direction
gameLost : Grid -> Bool
gameLost g =
    let
      up = fst <| slideGrid Up g
      down = fst <| slideGrid Down g
      left = fst <| slideGrid Left g
      right = fst <| slideGrid Right g
    in
    List.foldl (\x y -> x && y) True
      [ g /= emptyGrid
      , up == down
      , down == left
      , left == right
      , right == g
      ]


gameWon : Grid -> Bool
gameWon g =
  0 /= (List.length <| List.filter (\t -> t == Number 2048) <| List.concat g)


lose : GameState -> GameState
lose gameState =
  { gameState
  | gameProgress = GameOver
  }


win : GameState -> GameState
win gameState =
  { gameState
  | gameProgress = Won
  }


{------------------------------------------------------------------------------
                             Random tile placement
------------------------------------------------------------------------------}

tile2Probability : Float -- the probability that a new tile is a 2.
                         -- equivalently, the probability that a new
                         -- tile is a 4
tile2Probability = 0.9

newTile : Float -> Tile -- based on a float that will be random,
                        -- return a new tile
newTile x = if (x < tile2Probability) then (Number 2) else (Number 4)


-- a list of the coordinates of the empty tiles in a grid
emptyTiles : Grid -> List (Int, Int)
emptyTiles g = List.map (\(_,i,j) -> (i,j))
  <| List.filter (\(t,_,_) -> t == Empty)
  <| tilesWithCoordinates g


-- based on a float that will be random
-- return Just the coordinates of an empty tile in a
-- grid if one exists, or Nothing if there are none
newTileIndex : Float -> Grid -> Maybe (Int, Int)
newTileIndex x g =
    let
      emptyTileIndices = emptyTiles g
    in
    case emptyTileIndices of
      [] ->
        Nothing
      _ ->
        Just
          (Maybe.withDefault (0,0) (getAt emptyTileIndices
           (floor <| (toFloat <| List.length emptyTileIndices) * x)))


placeRandomTile : Float -> Float -> GameState -> GameState
placeRandomTile float1 float2 gameState =
    let
      tileIndex = newTileIndex float1 gameState.grid
    in
    if (tileIndex == Nothing) then
      gameState
    else
      { gameState
      | grid = setTile
        (Maybe.withDefault (0,0) tileIndex)
        gameState.grid
        (newTile float2)
      }


newGame : Input -> GameState
newGame input =
  let
    randoms = [0,1,2,3]
      |> List.map (getAt input.randomFloats)
      |> List.map (Maybe.withDefault 0)
    (i1,i2,i3,i4) = (0, 0.4, 0.6, 0.1)
  in
    placeRandomTile i1 i2
 <| placeRandomTile i3 i4
 <| defaultGame


stepGame : Input -> GameState -> GameState
stepGame input gameState =
    if input.controls.newGameButtonPressed then

      newGame input

    else if gameState.gameProgress /= InProgress then

      gameState

    else if gameWon gameState.grid then

      win gameState

    else if gameLost gameState.grid then

      lose gameState

    else if input.controls.tilePushDirection /= None then

      let
        pushedState = slideGameState input gameState
      in
      if (pushedState == gameState) then
        gameState
      else
        placeRandomTile
          (Maybe.withDefault 0 (getAt input.randomFloats 0))
          (Maybe.withDefault 0 (getAt input.randomFloats 1))
          pushedState

    else

      gameState
