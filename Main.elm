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
import Set
import Random
import Dict
import Debug

type alias Counter = Dict.Dict Coords Int

add : Coords -> Counter -> Counter
add coords counter = Dict.update coords (\count -> Just <| (Maybe.withDefault 0 count) + 1) counter

addDead : Coords -> Counter -> Counter
addDead coords counter = Dict.update coords (\count -> Just <| (Maybe.withDefault 0 count)) counter

counter : Counter
counter = Dict.empty

getCount : Coords -> Counter -> Int
getCount coord counter = Maybe.withDefault 0 <| Dict.get coord counter

type State = Living | Dead
type Update = Tick Float | MouseClick Coords | WindowSizeChange (Int, Int)
type alias Coords = (Int, Int)
type alias Origin = (Int, Int)
type alias Cell = {
    state : State
  }
type StateChange = Birth | Death | StayAlive | StayDead
type alias Grid = {
    cells : Dict.Dict (Int, Int) Cell, -- All cells
    living : Set.Set Coords, -- All currently living cells
    nrLivingNeighbours : Counter, -- Counts the number of times each cell was counted as a neighbour to a an already living cell
    changed : List (Coords, StateChange), -- Changes made during the current iteration
    dimensions : (Int, Int),
    fullRender : Bool
  }

xytoRowCol : (Int, Int) -> (Int, Int) -> Coords
xytoRowCol (w', h') (x, y) = let
                                fromMiddleX = (toFloat x) - (toFloat w')/2
                                fromMiddleY = (toFloat y) - (toFloat h')/2
                                r = fromMiddleY / cellSizeFull
                                c = fromMiddleX / cellSizeFull
                             in (-(round r), round c)

newCell : State -> Cell
newCell state = {state = state}

initGrid : Int -> Int -> Grid
initGrid r c = { living = Set.empty,
                 nrLivingNeighbours = counter,
                 changed = [],
                 dimensions = (30,30),
                 cells = Dict.empty,
                 fullRender = True}
                -- |> apply (10,10) glider
                -- |> apply (15,10) glider
                -- |> apply (10,15) glider
                -- |> apply (15,15) glider

type alias Creature = List Coords

fromJust : Maybe a -> a
fromJust x = case x of
        Just y -> y
        Nothing -> Debug.crash "error: fromJust Nothing"

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
apply (baseR, baseC) creature grid = let
                    creatureCoords = (rebase (baseR, baseC) creature)
                    in foldr birth grid creatureCoords

glider = [(0,0), (0,1), (0,2), (1,2), (2,1)]
--get : Grid -> Coords -> Cell

countNeighbours : Grid -> Grid
countNeighbours grid =
            Debug.watchSummary "nrLivingNeighbours" (\g -> g.nrLivingNeighbours)
            <| Debug.watchSummary "living" (\g -> g.living)
            <| Set.foldr scanCell grid grid.living

log a = Debug.log "logging" a

scanCell : Coords -> Grid -> Grid
scanCell coords grid = let
                    cell = Dict.get coords grid.cells
                    in case cell of
                    Just cell ->
                      let
                        neighbs = neighbours coords
                        neighbCells = List.filterMap (\n -> Dict.get n grid.cells) neighbs
                        newNrLivingNeighbours =  foldr add (addDead coords grid.nrLivingNeighbours) neighbs
                      in {grid | nrLivingNeighbours <- newNrLivingNeighbours}
                    Nothing -> grid

applyRules : Grid -> Grid
applyRules grid = let
                      currentState = (\coords -> if alive coords grid then Living else Dead)
                      changes = List.filter (\ (coords, change) -> change == Birth || change == Death) <| List.map (\ (coords, nrNeighbours) -> (coords, rules (currentState coords) nrNeighbours)) <| Dict.toList grid.nrLivingNeighbours
                  in {grid | changed <- List.append grid.changed changes}

kill : Coords -> Grid -> Grid
kill coords grid = {grid | cells <- Dict.remove coords grid.cells,
                           living <- Set.remove coords grid.living}

birth : Coords -> Grid -> Grid
birth coords grid = {grid | cells <- Dict.insert coords (newCell Living) grid.cells,
                            living <- Set.insert coords grid.living}

pruneDead : Grid -> Grid
pruneDead grid = let dead = map fst <| List.filter (\ (coords, change) -> change == Death) grid.changed
                     newGrid = foldr kill grid dead
                 in newGrid

birthNew : Grid -> Grid
birthNew grid = let birthed = map fst <| List.filter (\ (coords, change) -> change == Birth) grid.changed
                    newGrid = foldr birth grid birthed
              in newGrid

newIteration : Grid -> Grid
newIteration grid = {grid | changed <- [], nrLivingNeighbours <- counter, fullRender <- False}
-- newIteration grid = {grid | nrLivingNeighbours <- counter}

nextGrid : Update -> Grid -> Grid
nextGrid input grid = let
  newFullRender = case input of
            MouseClick (r,c) ->
                False
            Tick delta ->
                False
            WindowSizeChange (x, y) ->
                True
  newGrid = case input of
            MouseClick (r,c) ->
                birth (r,c) grid
            Tick delta ->
                grid
                |> newIteration
                |> countNeighbours
                |> applyRules
                |> pruneDead
                |> birthNew
            WindowSizeChange (x, y) ->
                {grid | dimensions <- (x,y)}
  in {newGrid | fullRender <- newFullRender}

aliveCell : Cell -> Bool
aliveCell cell = cell.state == Living

alive : Coords -> Grid -> Bool
alive coords grid = Set.member coords grid.living

rules : State -> Int -> StateChange
rules oldState nrNeighbours =
                          case oldState of
                                    Living -> if nrNeighbours < 2 || nrNeighbours > 3 then Death else StayAlive
                                    Dead -> if nrNeighbours == 3 then Birth else StayDead

-- applyChange : Grid -> Cell -> Coords -> StateChange -> Cell


neighbours : Coords -> List Coords
neighbours (r, c) = [
            (r-1, c-1), (r-1, c), (r-1, c+1),
            (r, c-1), (r, c+1),
            (r+1, c-1), (r+1, c), (r+1, c+1)]

colorLive = rgb 0xFF 0xFF 0xFF
colorDead = rgb 0x00 0x00 0x00
colorBackground = rgb 0x22 0x22 0x22

cellBottomLeft : (Int, Int) -> (Int, Int)
-- cellBottomLeft (w, h) = ((-w//2), (-h//2)) -- coordinate system of the collage is in "physics form" with (0,0) being in the middle, and y is up.
cellBottomLeft (w, h) = (0, 0) -- coordinate system of the collage is in "physics form" with (0,0) being in the middle, and y is up.

cellSize = 20.0
cellPadding = 0.5
cellSizeFull = cellSize + cellPadding

cellColor : Cell -> Color
cellColor cell = if aliveCell cell  then colorLive else colorDead

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

renderForms : (Int, Int) -> List Form -> Element
renderForms (w', h') forms = collage w' h' <| forms

renderCells: Origin -> List (Coords, Cell) -> List Form
renderCells origin cells = cells
                         |> map (\ (coords, cell) -> renderCell coords origin cell)

toDimensions : (Int, Int) -> (Int, Int)
toDimensions (w', h') = (w'// (round cellSizeFull), h'//(round cellSizeFull))

renderBackground : (Int, Int) -> List Form
renderBackground (w', h') = (filled colorBackground (rect (toFloat w') (toFloat h'))) :: (renderCells (cellBottomLeft (w', h')) <| deadCells (toDimensions (w',h')))

deadCells: (Int, Int) -> List (Coords, Cell)
deadCells (r, c) = List.concat <| for [-r..r-1] <| \ri ->
                for [-c..c-1] <| \ci ->
                  ((ri, ci), (newCell Dead))


renderFull : (Int, Int) -> Grid -> Element
renderFull (w', h') grid = renderForms (w', h') <| List.append (renderBackground (w', h')) (renderCells (cellBottomLeft (w', h')) <| log <| pairWithCells (Set.toList grid.living) grid)

renderChanges : (Int, Int) -> Grid -> Element
renderChanges (w', h') grid = renderForms (w', h') (renderCells (cellBottomLeft (w', h')) <| log <| pairWithCells (map fst <| log <| grid.changed) grid)
-- renderChanges : (Int, Int) -> Grid -> Element
-- renderChanges (w', h') grid = let a = log grid.changed
--   in renderForms (w', h') <| List.append (renderBackground (w', h')) <| renderCells (cellBottomLeft (w', h')) <| pairWithCells (Set.toList grid.living) grid

render : (Int, Int) -> Grid -> Element
render wSize grid = case grid.fullRender of
                      True ->
                        renderFull wSize grid
                      False ->
                        renderChanges wSize grid

pairWithCells : List Coords -> Grid -> List (Coords, Cell)
pairWithCells coordsList grid = map (\ (coords, mcell) -> (coords, fromJust mcell)) <| List.filter (\ (coords, mcell) -> isJust mcell) <| List.map (\c -> (c ,Dict.get c grid.cells)) coordsList

isJust : Maybe a -> Bool
isJust m = case m of
             Just a -> True
             Nothing -> False

input : Signal Update
input = let
    clicked = clickedCells
    clCells = Signal.map MouseClick <| Signal.dropRepeats <| clicked
    ticks = (Signal.map Tick (Time.fpsWhen 2 <| Signal.map not <| (Time.second `Time.since` clicked)))
    sizeChanges = Signal.map WindowSizeChange <| Signal.dropRepeats <| Window.dimensions
    in Signal.mergeMany [sizeChanges, clCells, ticks]

clickedCells : Signal Coords
clickedCells = let
  clicks = clickPositions
  currentDimensions = Signal.sampleOn clicks Window.dimensions
  in xytoRowCol <~ currentDimensions ~ clicks

clickPositions : Signal (Int, Int)
clickPositions = (Signal.sampleOn Mouse.clicks Mouse.position)

main : Signal Element
main = render <~ Window.dimensions ~ (foldp nextGrid (initGrid 70 50) input)
