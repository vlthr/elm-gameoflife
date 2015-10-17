module Game where
import List exposing (map, foldr, head, isEmpty, indexedMap, repeat, concat)
import Set
import Dict
import Debug
import Util exposing (log, for, isJust, fromJust)

type State = Living | Dead
type Update = Tick Float | MouseClick Coords | WindowSizeChange (Int, Int) | TogglePause
type alias Coords = (Int, Int)
type alias Origin = (Int, Int)
type alias Cell = {
    state : State
}
type StateChange = Birth | Death | StayAlive | StayDead
type alias Grid = {cells : Dict.Dict (Int, Int) Cell, -- All cells
    living : Set.Set Coords, -- All currently living cells
    nrLivingNeighbours : Counter, -- Counts the number of times each cell was counted as a neighbour to a an already living cell
    changed : List (Coords, StateChange), -- Changes made during the current iteration
    dimensions : (Int, Int),
    fullRender : Bool,
    paused : Bool
  }
type alias Counter = Dict.Dict Coords Int
type alias Creature = List Coords

rebase : Coords -> Creature -> Creature
rebase (baseR, baseC) creature = (List.map ( \ (r, c) -> (r+baseR, c+baseC)) creature)

apply (baseR, baseC) creature grid = let
  creatureCoords = (rebase (baseR, baseC) creature)
  in List.foldr birth grid creatureCoords

glider = [(0,0), (0,1), (0,2), (1,2), (2,1)]

add : Coords -> Counter -> Counter
add coords counter = Dict.update coords (\count -> Just <| (Maybe.withDefault 0 count) + 1) counter

addDead : Coords -> Counter -> Counter
addDead coords counter = Dict.update coords (\count -> Just <| (Maybe.withDefault 0 count)) counter

counter : Counter
counter = Dict.empty

getCount : Coords -> Counter -> Int
getCount coord counter = Maybe.withDefault 0 <| Dict.get coord counter

newCell : State -> Cell
newCell state = {state = state}

initGrid : Int -> Int -> Grid
initGrid r c = { living = Set.empty,
                 nrLivingNeighbours = counter,
                 changed = [],
                 dimensions = (r,c),
                 cells = Dict.empty,
                 fullRender = True,
                 paused = True}
-- |> apply (10,10) glider
                -- |> apply (15,10) glider
                -- |> apply (10,15) glider
                -- |> apply (15,15) glider

countNeighbours : Grid -> Grid
countNeighbours grid = Set.foldr scanCell grid grid.living


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

aliveCell : Cell -> Bool
aliveCell cell = cell.state == Living

alive : Coords -> Grid -> Bool
alive coords grid = Set.member coords grid.living

rules : State -> Int -> StateChange
rules oldState nrNeighbours =
                          case oldState of
                                    Living -> if nrNeighbours < 2 || nrNeighbours > 3 then Death else StayAlive
                                    Dead -> if nrNeighbours == 3 then Birth else StayDead

neighbours : Coords -> List Coords
neighbours (r, c) = [
            (r-1, c-1), (r-1, c), (r-1, c+1),
            (r, c-1), (r, c+1),
            (r+1, c-1), (r+1, c), (r+1, c+1)]


deadCells: (Int, Int) -> List (Coords, Cell)
deadCells (r, c) = List.concat <| for [-r..r-1] <| \ri ->
                for [-c..c-1] <| \ci ->
                  ((ri, ci), (newCell Dead))

pairWithCells : List Coords -> Grid -> List (Coords, Cell)
pairWithCells coordsList grid = map (\ (coords, mcell) -> (coords, fromJust mcell)) <| List.filter (\ (coords, mcell) -> isJust mcell) <| List.map (\c -> (c ,Dict.get c grid.cells)) coordsList


nextGrid : Update -> Grid -> Grid
nextGrid input grid = let
    newFullRender = case input of
            WindowSizeChange (x, y) ->
                True
            Tick delta ->
                False
            otherwise ->
            grid.fullRender
    newGrid = case input of
                MouseClick (r,c) ->
                    birth (r,c) grid
                Tick delta ->
                    case grid.paused of
                        False ->
                            grid
                            |> newIteration
                            |> countNeighbours
                            |> applyRules
                            |> pruneDead
                            |> birthNew
                        True ->
                            grid
                WindowSizeChange (x, y) ->
                    {grid | dimensions <- (x,y)}
                TogglePause ->
                    {grid | paused <- not grid.paused}
    in {newGrid | fullRender <- newFullRender}
