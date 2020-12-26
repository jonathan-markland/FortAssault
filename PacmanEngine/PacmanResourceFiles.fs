module PacmanResourceFiles

open ImagesAndFonts
open Sounds
open Geometry

let private image colourKey fileName width height =
    {
        ImageTransparency = colourKey
        ImageFileName  = fileName
        ImageWidth     = width  |> IntToIntEpx
        ImageHeight    = height |> IntToIntEpx
    }

let PacmanFontResourceImages =
    [
        image MagentaColourKeyImage "PacmanFont.png"     296 8
    ]

let PacmanResourceImages =
    [
        image OpaqueImage           "PacmanBackground.png"   320 256
        image MagentaColourKeyImage "PacmanLevel1.png"       544  16
        image OpaqueImage           "PacmanBackground2.png"  320 256
        image OpaqueImage           "PacmanBackground3.png"  320 256
    ]

let PacmanResourceSounds : SoundMetadata list =
    [
    ]

