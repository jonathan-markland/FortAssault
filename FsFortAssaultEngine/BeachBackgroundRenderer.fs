module BeachBackgroundRenderer

open Geometry
open ImagesAndFonts
open DrawingFunctions
open Time
open StaticResourceAccess



let RenderBeachBackground render (gameTime:float32<seconds>) =

    // TODO: We need repeat-tile-rendering support for this!
    // TODO: constants throughout this routine

    let mutable x = (((int (gameTime * 5.0F)) % 89) - 89) |> IntToIntEpx

    let imgCliffs = CliffsTileImageID |> ImageFromID

    for i in 1..5 do
        Image1to1 render x 0<epx> imgCliffs
        x <- x + 89<epx>


