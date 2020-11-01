module PacmanShared

open ResourceIDs
open Geometry
open Time
open DrawingShapes

type FacingDirection = FacingLeft | FacingRight | FacingUp | FacingDown

type GhostNumber = 
    /// Zero-based ghost number index type
    GhostNumber of int

let SnapsPerSecond        = 8.0F
let EyesTwitchesPerSecond = 2.0F

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  DATA MODELS
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type PacMode = 
    | PacAlive 
    | PacDyingUntil of float32<seconds>
    | PacDead

type PacState2 =
    {
        PacMode            : PacMode
        PacFacingDirection : FacingDirection
        LivesLeft          : int
    }

type PacmanState =
    {
        PacState2          : PacState2
        PacPosition        : PointI32
    }

type GhostMode =

    /// Ghost is displayed in solid colour and will lose pacman a life.
    | GhostNormal

    /// Ghost is edible until the given time.
    /// If eaten during this time, it will return to base.
    | GhostEdibleUntil of float32<seconds>

    /// Ghost is returning to base, and will change to GhostRegeneratingUntil
    /// state when it gets to its GhostHomePosition spot.
    | GhostReturningToBase

    /// Ghost is at its home spot, and is re-generating until the
    /// given game time whereupon it returns to GhostNormal.
    | GhostRegeneratingUntil of float32<seconds>

type GhostState2 =
    {
        /// This ghost's number, for convenient reference.
        /// May be used to determine behaviours.
        GhostNumber    : GhostNumber

        /// Ghost position at start of screen, used to 
        /// return to base after being eaten.
        GhostHomePosition : PointI32

        /// Ghost state.
        GhostMode      : GhostMode
    }

type GhostState =
    {
        GhostState2    : GhostState2
        GhostPosition  : PointI32
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  PREDICATES
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let IsGhostModeEdible ghostMode =
    match ghostMode with 
        | GhostEdibleUntil _ -> true
        | _ -> false

let AnyGhostsAreEdibleIn ghostList =
    ghostList |> List.exists (fun ghost -> ghost.GhostState2.GhostMode |> IsGhostModeEdible)



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  DRAWING
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DrawPacTileInt render image x y (tileIndex:int) =

    if tileIndex > 0 then  // tile index 0 is the blank tile

        let xIndex = tileIndex - 1

        render (
            DrawSubImageStretchedToTarget (
                xIndex * 16, 0, 16, 16,
                (x |> IntToFloatEpx), 
                (y |> IntToFloatEpx), 
                16<epx>, 
                16<epx>,
                image)) 

let private DrawPacTile render image x y (tileIndex:TileIndex) =

    let ti = ((int) tileIndex)
    DrawPacTileInt render image x y ti

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DrawPacMan render image originx originy pacmanState pillMode (gameTime:float32<seconds>) =

    let { ptix=cx ; ptiy=cy } = pacmanState.PacPosition

    let x = cx - (TileSide / 2) + originx
    let y = cy - (TileSide / 2) + originy

    let index =
        (int) (match pacmanState.PacState2.PacFacingDirection with 
                | FacingLeft  -> TileIndex.PacLeft1
                | FacingRight -> TileIndex.PacRight1
                | FacingUp    -> TileIndex.PacUp1
                | FacingDown  -> TileIndex.PacDown1)

    let pillModeFactor = if pillMode then 2.0F else 1.0F

    let n = ((int)(gameTime * SnapsPerSecond * pillModeFactor)) &&& 1

    let index = if n = 0 then index else index + 4

    DrawPacTileInt render image x y index

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DrawGhost render image originx originy ghostState (gameTime:float32<seconds>) =

    let { ptix=cx ; ptiy=cy } = ghostState.GhostPosition
    let (GhostNumber(ghostNumber)) = ghostState.GhostState2.GhostNumber

    let x = cx - (TileSide / 2) + originx
    let y = cy - (TileSide / 2) + originy

    DrawPacTileInt render image x y (ghostNumber + (int) TileIndex.Ghost1)

    let n = ((int)(gameTime * EyesTwitchesPerSecond * (float32 (ghostNumber + 1)))) &&& 1
    DrawPacTileInt render image x y (n + (int) TileIndex.Eyes1)

