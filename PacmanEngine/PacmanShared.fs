﻿module PacmanShared

open ResourceIDs
open Geometry
open Time
open DrawingShapes
open MazeFilter
open Rules

// TODO: library?

/// Returns an integer value that switches between 0 and 1, at a given rate.
let inline UnitPulse (rate:float32) (gameTime:float32<seconds>) = 
    ((int)(gameTime * rate)) &&& 1

/// Returns a value that switches between a given low and high, at a given rate.
let PulseBetween (rate:float32) low high (gameTime:float32<seconds>) = 
    if (UnitPulse rate gameTime) = 0 then low else high

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

// TODO: library?

type FacingDirection = FacingRight | FacingDown | FacingLeft | FacingUp

let FacingDirectionToMazeByte facingDirection = 
    match facingDirection with
        | FacingLeft  -> MazeByteLeft
        | FacingRight -> MazeByteRight
        | FacingUp    -> MazeByteUp
        | FacingDown  -> MazeByteDown

let FacingDirectionToInt facingDirection =
    match facingDirection with
        | FacingLeft  -> 0
        | FacingUp    -> 1
        | FacingRight -> 2
        | FacingDown  -> 3

type Angle = ZeroAngle | ClockwiseTurn90 | AboutTurn180 | AntiClockwiseTurn90

let IntToAngle i = 
    match i with
        | 0 -> ZeroAngle
        | 1 -> ClockwiseTurn90
        | 2 -> AboutTurn180
        | 3 -> AntiClockwiseTurn90
        | _ -> failwith "Invalid integer value for conversion to type Angle"

let AngleBetween current previous =
    let d = (current |> FacingDirectionToInt) - (previous |> FacingDirectionToInt)
    (d &&& 3) |> IntToAngle

let inline DirectionToMovementDelta zero i facingDirection =
    match facingDirection with
        | FacingLeft  -> { modx = -i    ; mody =  zero }
        | FacingRight -> { modx =  i    ; mody =  zero }
        | FacingUp    -> { modx =  zero ; mody = -i    }
        | FacingDown  -> { modx =  zero ; mody =  i    }

let DirectionToMovementDeltaI32 =
    DirectionToMovementDelta 0<epx> 1<epx> 

let KeyStatesToDirection u d l r defaultDirection =
    // These are to be exclusive.
    if   u=true  && d=false && l=false && r=false then FacingUp
    elif u=false && d=true  && l=false && r=false then FacingDown
    elif u=false && d=false && l=true  && r=false then FacingLeft
    elif u=false && d=false && l=false && r=true  then FacingRight
    else defaultDirection

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 


type GhostNumber = 
    /// Zero-based ghost number index type
    GhostNumber of int

let SnapsPerSecond        = 8.0F
let EyesTwitchesPerSecond = 2.0F

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  DATA MODELS
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type PacMode = 

    /// Pacman is alive and controlled by player.
    /// Can always do a 180 degree turn.  Can only turn 90 degrees when
    /// aligned on the major grid, and as the maze allows.  Pac travels
    /// until hitting a dead end or 90 degree corner where he stops.
    | PacAlive 

    /// Pacman flashing during death phase.
    /// Player cannot control during this.
    | PacDyingUntil of float32<seconds>

    /// Pacman is absent from the screen, and the restart logic kicks in at the game time.
    /// Player cannot control during this.
    | PacDeadUntil of float32<seconds>

type PacState2 =
    {
        PacMode            : PacMode
        PacFacingDirection : FacingDirection
        LivesLeft          : int
    }

type PacmanState =
    {
        PacState2          : PacState2

        /// Stored position is relative to top left of maze.
        PacPosition        : Point<int<epx>>
    }

    // Reminder: "Pill mode" is NOT a state of pacman himself.  If any of the ghosts
    //           are running down their "edible mode" timers, than pacman is in pill mode.

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

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
        /// Stored position is relative to top left of maze.
        GhostHomePosition : Point<int<epx>>

        /// Travel direction
        GhostFacingDirection : FacingDirection

        /// Ghost state.
        GhostMode      : GhostMode
    }

type GhostState =
    {
        GhostState2    : GhostState2

        /// Stored position is relative to top left of maze.
        GhostPosition  : Point<int<epx>>
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  PREDICATES
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let IsGhostModeEdible ghostMode =
    match ghostMode with 
        | GhostEdibleUntil _ -> true
        | _ -> false

/// Asks whether the game is in "pill mode", which is the case
/// for pacman if any of the ghosts are currently edible.  Each
/// ghost must be individually considered because some may be
/// in normal mode (after regeneration) while others in edible mode.
let InPillMode ghostList =
    ghostList |> List.exists (fun ghost -> ghost.GhostState2.GhostMode |> IsGhostModeEdible)



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  DRAWING
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DrawPacTileInt render image x y (tileIndex:int) gameTime =

    if tileIndex > 0 then  // tile index 0 is the blank tile

        let xIndex =
            if tileIndex = ((int) TileIndex.Pill1) then
                gameTime |> PulseBetween 10.0F ((int) TileIndex.Pill1) ((int) TileIndex.Pill2)
            else
                tileIndex

        render (
            DrawSubImageStretchedToTarget (
                (xIndex - 1) * 16, 0, 16, 16,   // subtract 1 because we don't store image data for the blank tile.
                (x |> IntEpxToInt |> IntToFloatEpx), 
                (y |> IntEpxToInt |> IntToFloatEpx), 
                16<epx>, 
                16<epx>,
                image)) 

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Draw pac man image with top left at pos facing in the direction given,
/// with appropriate open/closed mouth animation.
let DrawPacMan render image pos facingDirection pillMode (gameTime:float32<seconds>) =

    let { ptx=x ; pty=y } = pos

    let pacDirectionalImageIndex =
        (int) (match facingDirection with 
                | FacingLeft  -> TileIndex.PacLeft1
                | FacingRight -> TileIndex.PacRight1
                | FacingUp    -> TileIndex.PacUp1
                | FacingDown  -> TileIndex.PacDown1)

    let pillModeFactor = if pillMode then 2.0F else 1.0F

    let pacImageIndex = 
        gameTime |> PulseBetween 
            (SnapsPerSecond * pillModeFactor)
            pacDirectionalImageIndex (pacDirectionalImageIndex + 4) 

    DrawPacTileInt render image x y pacImageIndex gameTime

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Draw ghost image with top left at pos, selecting the appropriate
/// ghost colour and eyes animation frame.
let DrawGhost render image pos (GhostNumber(ghostNumber)) ghostMode (gameTime:float32<seconds>) =

    let { ptx=x ; pty=y } = pos

    let ghostImageIndex =
        
        let normal = ghostNumber + (int) TileIndex.Ghost1
        let dark = (int) TileIndex.GhostReturning
        let pale = (int) TileIndex.GhostPale

        match ghostMode with
            | GhostNormal ->
                normal

            | GhostEdibleUntil t ->
                if (t - gameTime) < PowerPillWarnTime then
                    gameTime |> PulseBetween PowerPillWarnFlashRate pale normal
                else
                    pale

            | GhostReturningToBase ->
                dark

            | GhostRegeneratingUntil _ ->
                gameTime |> PulseBetween RegenerationFlashRate dark normal

    DrawPacTileInt render image x y ghostImageIndex gameTime

    let wiggleRate = EyesTwitchesPerSecond * (float32 (ghostNumber + 1))
    let eyes = gameTime |> PulseBetween wiggleRate TileIndex.Eyes1 TileIndex.Eyes2

    DrawPacTileInt render image x y ((int) eyes) gameTime

