module Main where
import Html exposing (div, button, text)
import Html.Events exposing (onClick)
import Window
import Signal exposing (sampleOn, foldp, (~), (<~))
import Time exposing (fps)
import Graphics.Element exposing (Element, tiledImage)
import Graphics.Collage exposing (Form, move, group, filled, circle, toForm, traced, collage, solid, rect)
import Color exposing (Color, lightRed, lightYellow, lightBlue, white, red, blue, rgb)
import Touch
import Mouse
import Keyboard
import Random
import List exposing (map, foldr, head, isEmpty, indexedMap, repeat, concat)
import Set
import Dict
import Debug
import Util exposing (log, for, isJust, fromJust)
import Game exposing (..)


main: Signal Element
main = render <~ Window.dimensions ~ (foldp nextGrid (initGrid 70 50) input)

--
-- Helper functions
--
xytoRowCol : (Int, Int) -> (Int, Int) -> Coords
xytoRowCol (w', h') (x, y) = let
    fromMiddleX = (toFloat x) - (toFloat w')/2
    fromMiddleY = (toFloat y) - (toFloat h')/2
    r = fromMiddleY / cellSizeFull
    c = fromMiddleX / cellSizeFull
    in (-(round r), round c)

colorLive = rgb 0xFF 0xFF 0xFF
colorDead = rgb 0x00 0x00 0x00
colorBackground = rgb 0x22 0x22 0x22

cellOrigin : (Int, Int) -> (Int, Int)
cellOrigin (w, h) = (0, 0) -- coordinate system of the collage is in "physics form" with (0,0) being in the middle, and y is up.

cellSize = 20.0
cellPadding = 0.5
cellSizeFull = cellSize + cellPadding

cellColor : Cell -> Color
cellColor cell = if aliveCell cell  then colorLive else colorDead

--
-- Rendering
--
renderFull : (Int, Int) -> Grid -> Element
renderFull (w', h') grid = renderForms (w', h') <| List.append (renderBackground (w', h')) (renderCells (cellOrigin (w', h')) <| log <| pairWithCells (Set.toList grid.living) grid)

renderChanges : (Int, Int) -> Grid -> Element
renderChanges (w', h') grid = renderForms (w', h') (renderCells (cellOrigin (w', h')) <| log <| pairWithCells (map fst <| log <| grid.changed) grid)

render : (Int, Int) -> Grid -> Element
render wSize grid = case grid.fullRender of
                      True ->
                        renderFull wSize grid
                      False ->
                        renderFull wSize grid

renderCell : Coords -> Origin -> Cell -> Form
renderCell  (r, c) (x, y) cell = move ( (toFloat x) + (toFloat c) * cellSizeFull, (toFloat y) + (toFloat r) * cellSizeFull) (filled (cellColor cell) (rect cellSize cellSize))

renderForms : (Int, Int) -> List Form -> Element
renderForms (w', h') forms = collage w' h' <| forms

renderCells: Origin -> List (Coords, Cell) -> List Form
renderCells origin cells = cells
                         |> map (\ (coords, cell) -> renderCell coords origin cell)

toDimensions : (Int, Int) -> (Int, Int)
toDimensions (w', h') = (w'// (round cellSizeFull), h'//(round cellSizeFull))

renderBackground : (Int, Int) -> List Form
renderBackground (w', h') = (filled colorBackground (rect (toFloat w') (toFloat h'))) :: (renderCells (cellOrigin (w', h')) <| deadCells (toDimensions (w',h')))

--
-- Signals/Input
--
input : Signal Update
input = let
    clicked = clickedCells
    clCells = Signal.map MouseClick <| Signal.dropRepeats <| clicked
    ticks = (Signal.map Tick (Time.fpsWhen 2 <| Signal.map not <| (Time.second `Time.since` clicked)))
    sizeChanges = Signal.map WindowSizeChange <| Signal.dropRepeats <| Window.dimensions
    in Signal.mergeMany [sizeChanges, clCells, ticks, pauses]

pauses : Signal Update
pauses = let
    spacepressed = Signal.filter (\s -> s == True) False <| Signal.dropRepeats Keyboard.space
    toggles = Signal.constant TogglePause
    in Signal.sampleOn spacepressed toggles

clickedCells : Signal Coords
clickedCells = let
  clicks = clickPositions
  currentDimensions = Signal.sampleOn clicks Window.dimensions
  in xytoRowCol <~ currentDimensions ~ clicks

clickPositions : Signal (Int, Int)
clickPositions = (Signal.sampleOn Mouse.clicks Mouse.position)
