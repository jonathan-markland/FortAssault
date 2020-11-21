module PacmanShared

open ResourceIDs
open Geometry
open Time
open DrawingShapes
open Rules
open Directions
open GhostAI

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let KeyStatesToDirection u d l r defaultDirection =
    // These are to be exclusive.
    if   u=true  && d=false && l=false && r=false then FacingUp
    elif u=false && d=true  && l=false && r=false then FacingDown
    elif u=false && d=false && l=true  && r=false then FacingLeft
    elif u=false && d=false && l=false && r=true  then FacingRight
    else defaultDirection

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

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

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type GhostNumber = 
    /// Zero-based ghost number index type
    GhostNumber of int



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  PAC MAN DATA MODEL
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

// Reminder: "Pill mode" is NOT a state of pacman himself.  If any of the ghosts
//           are running down their "edible mode" timers, than pacman is in pill mode.

type PacMode = 

    /// Pacman is alive and controlled by player.
    /// Can always do a 180 degree turn.  Can only turn 90 degrees when
    /// aligned on the major grid, and as the maze allows.  Pac travels
    /// until hitting a dead end or 90 degree corner where he stops.
    | PacAlive 

    /// Pacman flashing during death phase.
    /// Player cannot control during this.
    | PacDyingUntil of float32<seconds>

    /// Signal main loop to decrement lives count and switch to life lost card.
    | PacDead



/// Pacman state that's less likely to change per frame.
type PacState2 =
    {
        PacMode            : PacMode
        PacFacingDirection : FacingDirection
        LivesLeft          : uint32
        PacStartPosition   : Point<int<epx>>
    }



/// Pacman state that very likely to change per frame.
type PacmanState =
    {
        PacState2          : PacState2

        /// Stored position is relative to top left of maze.
        PacPosition        : Point<int<epx>>
    }



/// Pacman mode property.
let inline PacMode pacman =
    pacman.PacState2.PacMode

/// Pacman's direction-facing property.
let inline Facing pacman =
    pacman.PacState2.PacFacingDirection

/// Pacman's start position property.
let inline StartPosition pacman =
    pacman.PacState2.PacStartPosition

/// Pacman life is over property.
let inline LifeIsOver pacman =
    match pacman |> PacMode with
        | PacDead -> true
        | _ -> false

/// The number of lives pacman has remaining.
let inline LivesLeft pacman =
    pacman.PacState2.LivesLeft

/// Is the game over for pacman?
let inline GameIsOver pacman =
    pacman |> LivesLeft <= 1u



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  GHOST DATA MODEL
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



/// Ghost state that's less likely to change per frame.
type GhostState2 =
    {
        /// This ghost's number, for convenient reference.
        /// May be used to determine behaviours.
        GhostTag : GhostNumber

        /// Ghost position at start of screen, used to 
        /// return to base after being eaten.
        /// Stored position is relative to top left of maze.
        GhostBasePosition : Point<int<epx>>

        /// Travel direction, must be set correctly
        /// with respect to rails.
        GhostFacingDirection : FacingDirection

        /// Ghost state.
        GhostMode : GhostMode

        /// The direction choice traits for this ghost.
        GhostAITable : GhostAI
    }



/// Ghost state that's very likely to change per frame.
type GhostState =
    {
        GhostState2    : GhostState2

        /// Stored position is relative to top left of maze.
        GhostPosition  : Point<int<epx>>
    }



/// Ghost's mode property.
let inline GhostMode ghost =
    ghost.GhostState2.GhostMode

/// The position of this ghost's square in the base.
let inline BasePosition ghost =
    ghost.GhostState2.GhostBasePosition

/// The direction the ghost is going.
let inline GlideDirection ghost =
    ghost.GhostState2.GhostFacingDirection

/// This ghost's tag number.  Used as identity.
let inline Tag ghost =
    ghost.GhostState2.GhostTag

/// Ghost AI
let inline AIFor ghost =
    ghost.GhostState2.GhostAITable

/// Asks whether this ghost is the same instance as another.
let inline IsTheSameGhostAs ghost otherGhost =
    (ghost |> Tag) = (otherGhost |> Tag)


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

type PacDrawType = DrawPacNormal | DrawPacPillMode | DrawPacZapped

/// Draw pac man image with top left at pos facing in the direction given,
/// with appropriate open/closed mouth animation.
let DrawPacManAlive render image pos facingDirection pacDrawType (gameTime:float32<seconds>) =

    let { ptx=x ; pty=y } = pos

    let pacDirectionalImageIndex =
        (int) (match facingDirection with 
                | FacingLeft  -> TileIndex.PacLeft1
                | FacingRight -> TileIndex.PacRight1
                | FacingUp    -> TileIndex.PacUp1
                | FacingDown  -> TileIndex.PacDown1)

    let pacImageIndex = 

        let imageWithSnapSpeedTimes pillModeFactor =
            gameTime |> PulseBetween 
                (SnapsPerSecond * pillModeFactor)
                pacDirectionalImageIndex (pacDirectionalImageIndex + 4) 

        match pacDrawType with
            | DrawPacNormal   -> imageWithSnapSpeedTimes 1.0F
            | DrawPacPillMode -> imageWithSnapSpeedTimes 2.0F
            | DrawPacZapped ->
                pacDirectionalImageIndex

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
                    gameTime |> PulseBetween PowerPillWarnFlashRate normal dark
                else
                    gameTime |> PulseBetween EdibleFlashRate pale dark

            | GhostReturningToBase ->
                dark

            | GhostRegeneratingUntil _ ->
                gameTime |> PulseBetween RegenerationFlashRate dark normal

    DrawPacTileInt render image x y ghostImageIndex gameTime

    let wiggleRate = EyesTwitchesPerSecond * (float32 (ghostNumber + 1))
    let eyes = gameTime |> PulseBetween wiggleRate TileIndex.Eyes1 TileIndex.Eyes2

    DrawPacTileInt render image x y ((int) eyes) gameTime

