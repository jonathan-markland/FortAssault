module PacmanResourceFiles

open ImagesAndFonts
open Sounds

let private image colourKey fileName =
    {
        RequestedImageTransparency = colourKey
        RequestedImageFileName     = fileName
    }

let private font fileName charWidth =
    {
        RequestedFontImage     = image MagentaColourKeyImage fileName
        RequestedFontCharWidth = charWidth
    }

let private sound fileName =
    {
        RequestedSoundFileName = fileName
    }


let PacmanFontResourceImages =
    [
        font "PacmanFont.png" 8
    ]

let PacmanResourceImages =
    [
        image OpaqueImage           "PacmanBackground.png" 
        image MagentaColourKeyImage "PacmanLevel1.png"     
        image OpaqueImage           "PacmanBackground2.png"
        image OpaqueImage           "PacmanBackground3.png"
    ]

let PacmanResourceSounds =
    [
        sound "pellet.ogg" 
        sound "pill.ogg" 
        sound "321.ogg" 
        sound "game-over.ogg" 
        sound "go.ogg" 
        sound "gulp.ogg" 
        sound "oww.ogg" 
        sound "victory.ogg"
        sound "life.ogg"
    ]
