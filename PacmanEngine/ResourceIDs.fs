module ResourceIDs

open Geometry
open ImagesAndFonts


let TileSide = 16<epx>

/// These index the PacmanLevel PNG files.
type TileIndex =
    
    | Wall1  =  0
    | Wall2  =  1
    | Wall3  =  2
    | Wall4  =  3
    | Wall5  =  4
    | Wall6  =  5
    | Wall7  =  6
    | Wall8  =  7
    | Wall9  =  8
    | Wall10 =  9
    | Wall11 = 10
    | Wall12 = 11
    | Wall13 = 12
    | Wall14 = 13
    | Wall15 = 14

    | Dot    = 15
    | Pill1  = 16
    | Pill2  = 17

    | PacRight1 = 18
    | PacLeft1  = 19
    | PacUp1    = 20
    | PacDown1  = 21

    | PacRight2 = 22
    | PacLeft2  = 23
    | PacUp2    = 24
    | PacDown2  = 25

    | Ghost1    = 26
    | Ghost2    = 27
    | Ghost3    = 28
    | Ghost4    = 29

    | Eyes1  = 30
    | Eyes2  = 31

    | GhostPale = 32


// This game uses a 320*200 engine-coordinate (epx) space:

let ScreenWidthInt  = 320<epx>  // Must be the same as below!
let ScreenHeightInt = 256<epx>  // Must be the same as below!

let ScreenWidth  = 320.0F<epx>
let ScreenHeight = 256.0F<epx>

// Fonts (all screens):

let GreyFontID                  = FontID(0)

// Images (all screens):

let BackgroundImageID           = ImageID(0)
let Level1ImageID               = ImageID(1)
let Background2ImageID          = ImageID(2)

    


