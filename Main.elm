module Main where
import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import Window
import Signal exposing (sampleOn, foldp, (~), (<~))
import Time exposing (fps)
import Graphics.Element exposing (Element, tiledImage)
import Graphics.Collage exposing (Form, move, group, filled, circle, toForm, traced, collage, solid, rect)
import Color exposing (Color, lightRed, lightYellow, lightBlue, white, red, blue, rgb)
import List exposing (map, foldr, head, isEmpty, indexedMap, repeat, concat)
import Touch
import Mouse
import Random
import Dict

type alias State = Bool
type alias Input = (Int, Int)
type alias Coords = (Int, Int)
type alias Origin = (Int, Int)
type alias Cell = {
    state : State
  }
type alias Grid = {
    cells : Dict.Dict (Int, Int) Cell
  }

initGrid : Int -> Int -> Grid
initGrid r c = { cells = Dict.fromList (concat <| for [0..r-1] <| \ri ->
                                           for [0..c-1] <| \ci ->
                                               ((ri, ci), {state = False}))}
                |> apply (10,10) glider

type alias Creature = List Coords

updateIndexes : (a -> a) -> List (List a) -> List (Int, Int) -> List (List a)
updateIndexes fn list indexes = dblIndexedUpdate (\r c el -> if List.member (r,c) indexes then fn el else el) list

dblIndexedUpdate : (Int -> Int -> a -> b) -> List (List a) -> List (List b)
dblIndexedUpdate fn list = let -- elemFn = \r c elem -> fn r c elem
                rowFn rowIndex column = indexedMap (\c elem -> fn rowIndex c elem) column
                in indexedMap rowFn list

for : List a -> (a -> b) -> List b
for xs f = List.map f xs

rebase : Coords -> Creature -> Creature
rebase (baseR, baseC) creature = (map ( \ (r, c) -> (r+baseR, c+baseC)) creature)

apply : Coords -> Creature -> Grid -> Grid
apply (baseR, baseC) creature grid = {grid | cells <- updateKeys (\ (r, c) cell -> {cell | state <- True}) (rebase (baseR, baseC) creature) grid.cells }

glider = [(0,0), (0,1), (0,2), (1,2), (2,1)]
--get : Grid -> Coords -> Cell

nextGrid : Input -> Grid -> Grid
nextGrid input grid = {grid | cells <- keyMap (\ (r, c) cell -> {cell | state <- nextState grid (r, c) }) grid.cells}

alive : Cell -> Bool
alive cell = cell.state

nextState : Grid -> Coords -> Bool
nextState grid coords = neighbours coords
                      |> List.filterMap (\neighbour -> Dict.get neighbour grid.cells)
                      |> List.filter alive
                      |> List.length
                      |> (<) 4


neighbours : Coords -> List Coords
neighbours (r, c) = [
            (r-1, c-1), (r-1, c), (r-1, c+1),
            (r, c-1), (r, c), (r, c+1),
            (r+1, c-1), (r+1, c), (r+1, c+1)]

colorLive = rgb 0xFF 0xFF 0xFF
colorDead = rgb 0x00 0x00 0x00
colorBackground = rgb 0x22 0x22 0x22

cellBottomLeft : (Int, Int) -> (Int, Int)
cellBottomLeft (w, h) = ((-w//2), (-h//2)) -- coordinate system of the collage is in "physics form" with (0,0) being in the middle, and y is up.

cellSize = 20.0
cellPadding = 0.5
cellSizeFull = cellSize + cellPadding

cellColor : Cell -> Color
cellColor cell = if cell.state  then colorLive else colorDead

renderCell : Coords -> Origin -> Cell -> Form
renderCell  (r, c) (x, y) cell = move ( (toFloat x) + (toFloat c) * cellSizeFull, (toFloat y) + (toFloat r) * cellSizeFull) (filled (cellColor cell) (rect cellSize cellSize))

dblIndexedMap : (Int -> Int -> a -> b) -> List (List a) -> List b
dblIndexedMap fn list = let -- elemFn = \r c elem -> fn r c elem
                rowFn rowIndex column = indexedMap (\c elem -> fn rowIndex c elem) column
                in concat (indexedMap rowFn list)

updateKeys : (comparable -> b -> b) -> List comparable -> Dict.Dict comparable b -> Dict.Dict comparable b
updateKeys fn keys dict = foldr (\key d -> Dict.update key (Maybe.map (fn key)) d) dict keys

-- Equivalent to updateKeys using all keys of the dict
keyMap : (comparable -> b -> b) -> Dict.Dict comparable b -> Dict.Dict comparable b
keyMap fn dict = updateKeys fn (Dict.keys dict) dict

renderGrid : (Int, Int) -> Grid -> Element
renderGrid (w', h') grid = collage w' h' <| (filled colorBackground (rect (toFloat w') (toFloat h'))) :: (map (\ ((r,c), cell) -> renderCell (r,c) (cellBottomLeft (w',h')) cell) (Dict.toList grid.cells))

input : Signal (Int, Int)
input = sampleOn Mouse.clicks Mouse.position

main : Signal Element
main = renderGrid <~ (sampleOn (fps 10) Window.dimensions) ~ (foldp nextGrid (initGrid 100 100) input)

-- main = Signal.map show Mouse.position


