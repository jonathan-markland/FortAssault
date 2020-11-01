module ResourceIDs

open Geometry
open ImagesAndFonts


let TileSide = 16<epx>
let TileSideInt = 16

/// These index the PacmanLevel PNG files.
type TileIndex =

    | Blank  =  0uy  // 
    | Wall0  =  1uy  //
    | Wall1  =  2uy  // 
    | Wall2  =  3uy  // 
    | Wall3  =  4uy  // 
    | Wall4  =  5uy  // 
    | Wall5  =  6uy  // 
    | Wall6  =  7uy  // 
    | Wall7  =  8uy  // 
    | Wall8  =  9uy  //    Must be a cluster
    | Wall9  = 10uy  // 
    | Wall10 = 11uy  // 
    | Wall11 = 12uy  // 
    | Wall12 = 13uy  // 
    | Wall13 = 14uy  // 
    | Wall14 = 15uy  // 
    | Wall15 = 16uy  // 

    | Dot    = 17uy
    | Pill1  = 18uy
    | Pill2  = 19uy

    | PacRight1 = 20uy
    | PacLeft1  = 21uy
    | PacUp1    = 22uy
    | PacDown1  = 23uy

    | PacRight2 = 24uy
    | PacLeft2  = 25uy
    | PacUp2    = 26uy
    | PacDown2  = 27uy

    | Ghost1    = 28uy
    | Ghost2    = 29uy
    | Ghost3    = 30uy
    | Ghost4    = 31uy

    | Eyes1     = 32uy
    | Eyes2     = 33uy

    | GhostPale = 34uy


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

    


