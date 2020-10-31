module ResourceIDs

open Geometry
open ImagesAndFonts


let TileSide = 16<epx>
let TileSideInt = 16

/// These index the PacmanLevel PNG files.
type TileIndex =

    | Blank  =  0uy  // 
    | Wall1  =  1uy  // 
    | Wall2  =  2uy  // 
    | Wall3  =  3uy  // 
    | Wall4  =  4uy  // 
    | Wall5  =  5uy  // 
    | Wall6  =  6uy  // 
    | Wall7  =  7uy  // 
    | Wall8  =  8uy  //    Must be a cluster
    | Wall9  =  9uy  // 
    | Wall10 = 10uy  // 
    | Wall11 = 11uy  // 
    | Wall12 = 12uy  // 
    | Wall13 = 13uy  // 
    | Wall14 = 14uy  // 
    | Wall15 = 15uy  // 

    | Dot    = 16uy
    | Pill1  = 17uy
    | Pill2  = 18uy

    | PacRight1 = 19uy
    | PacLeft1  = 20uy
    | PacUp1    = 21uy
    | PacDown1  = 22uy

    | PacRight2 = 23uy
    | PacLeft2  = 24uy
    | PacUp2    = 25uy
    | PacDown2  = 26uy

    | Ghost1    = 27uy
    | Ghost2    = 28uy
    | Ghost3    = 29uy
    | Ghost4    = 30uy

    | Eyes1     = 31uy
    | Eyes2     = 32uy

    | GhostPale = 33uy


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

    


