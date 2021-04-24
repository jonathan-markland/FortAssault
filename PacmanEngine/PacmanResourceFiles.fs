﻿module PacmanResourceFiles

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

let private font fileName width charWidth charHeight =
    {
        FontImageMetadata = image MagentaColourKeyImage fileName width charHeight
        FontCharWidth     = charWidth
    }

let PacmanFontResourceImages =
    [
        font "PacmanFont.png"     296 8 8
    ]

let PacmanResourceImages =
    [
        image OpaqueImage           "PacmanBackground.png"   320 256
        image MagentaColourKeyImage "PacmanLevel1.png"       560  16
        image OpaqueImage           "PacmanBackground2.png"  320 256
        image OpaqueImage           "PacmanBackground3.png"  320 256
    ]

let PacmanResourceSounds : SoundMetadata list =
    [
        { SoundFileName = "pellet.ogg" }
        { SoundFileName = "pill.ogg" }
        { SoundFileName = "321.ogg" }
        { SoundFileName = "game-over.ogg" }
        { SoundFileName = "go.ogg" }
        { SoundFileName = "gulp.ogg" }
        { SoundFileName = "oww.ogg" }
        { SoundFileName = "victory.ogg" }
        { SoundFileName = "life.ogg" }
    ]
