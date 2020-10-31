module DrawingShared

open ResourceIDs
open Geometry
open Time
open DrawingShapes

type FacingDirection = FacingLeft | FacingRight | FacingUp | FacingDown
type GhostNumber = GhostNumber of int

let SnapsPerSecond        = 8.0F
let EyesTwitchesPerSecond = 2.0F

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DrawPacTileInt render image x y (tileIndex:int) =

    render (
        DrawSubImageStretchedToTarget (
            tileIndex * 16, 0, 16, 16,
            (x |> IntToFloatEpx), 
            (y |> IntToFloatEpx), 
            16<epx>, 
            16<epx>,
            image)) 

let private DrawPacTile render image x y (tileIndex:TileIndex) =

    let ti = ((int) tileIndex)
    DrawPacTileInt render image x y ti

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DrawPacMan render image cx cy facingDirection pillMode (gameTime:float32<seconds>) =

    let x = cx - (TileSide / 2)
    let y = cy - (TileSide / 2)

    let index =
        (int) (match facingDirection with 
                | FacingLeft  -> TileIndex.PacLeft1
                | FacingRight -> TileIndex.PacRight1
                | FacingUp    -> TileIndex.PacUp1
                | FacingDown  -> TileIndex.PacDown1)

    let pillModeFactor = if pillMode then 2.0F else 1.0F

    let n = ((int)(gameTime * SnapsPerSecond * pillModeFactor)) &&& 1

    let index = if n = 0 then index else index + 4

    DrawPacTileInt render image x y index

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DrawGhost render image cx cy ghostNumber (gameTime:float32<seconds>) =

    let x = cx - (TileSide / 2)
    let y = cy - (TileSide / 2)

    let (GhostNumber(ghostNumber)) = ghostNumber
    DrawPacTileInt render image x y (ghostNumber + (int) TileIndex.Ghost1)

    let n = ((int)(gameTime * EyesTwitchesPerSecond * (float32 (ghostNumber + 1)))) &&& 1
    DrawPacTileInt render image x y (n + (int) TileIndex.Eyes1)

