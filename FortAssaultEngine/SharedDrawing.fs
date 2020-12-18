module SharedDrawing

open Geometry
open DrawingShapes
open DrawingFunctions
open ResourceIDs

let ScoreboardArea render contentHeight =
    Rectangle render 0<epx> contentHeight ScreenWidthInt (ScreenHeightInt - contentHeight) (SolidColour(0000000u))

