module ScreenPacman

open Time
open DrawingFunctions
open ScoreHiScore
open Geometry
open ResourceIDs
open StaticResourceAccess
open ScreenHandler
open ImagesAndFonts
open PacmanShared
open Input
open MazeFilter
open Rules
open Mazes
open MazeState
open ScreenIntermissions
open Random
open GhostDirectionChoosing
open Update
open Keys
open MazeUnpacker


let ScreenRandomSeed = 0x33033u

// TODO: Possibly treat corners the same as straight lines for ghost decision points,
//       with a completely separate probability setting for turn about.

// TODO: The ghosts that are less decisive may get trapped in the base because the
//       base rails are a square network.  This is only an issue for the early stage.

// TODO: Strong typing instead of "byte" all other the place.  Some work was started
//       in the library allowing the user to StringArrayToMazeArray tileMapper parameter.

// TODO: Research - a pure functional pacman maze instead of the array mutability.

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private PacmanScreenModel =  // TODO: Getting fat with things that don't change per-frame
    {
        Random                 : XorShift32State
        LevelNumber            : int
        ScoreAndHiScore        : ScoreAndHiScore
        MazeState              : MazeState
        PacmanState            : PacmanState
        GhostsState            : GhostState list
        WhereToOnGameOver      : ScoreAndHiScore -> ErasedGameState
        WhereToOnAllEaten      : int -> ScoreAndHiScore -> float32<seconds> -> ErasedGameState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Support functions
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Obtain key states as boolean values.
let KeysFrom keyStateGetter =
    let up    = (keyStateGetter KeyUp).Held
    let down  = (keyStateGetter KeyDown).Held
    let left  = (keyStateGetter KeyLeft).Held
    let right = (keyStateGetter KeyRight).Held
    struct (up, down, left, right)

/// Asks whether the given pacman or ghost position is aligned
/// to cover a single tile.
let IsAlignedOnTile position =

    let {ptx=x ; pty=y} = position
    let isAligned n = ((n |> IntEpxToInt) % TileSideInt) = 0

    x |> isAligned && y |> isAligned

/// For a precise pacman or ghost position that is precisely over
/// a single tile, this returns the rails array index of that tile.
/// If the position not perfectly aligned on a single tile, this
/// returns None.
let TileIndexOf position =  // TODO: strongly type the return value

    if position |> IsAlignedOnTile then
        let {ptx=x ; pty=y} = position
        let txi = x / TileSide
        let tyi = y / TileSide
        Some (txi, tyi)

    else
        None


let InitialFacingDirectionFor ghostPos (mazeGhostRails:byte[]) tileCountX =
    match TileIndexOf ghostPos with
        | None -> failwith "Cannot determine initial direction for ghost because initial ghost position is not precisely aligned"
        | Some (txi, tyi) ->
            let i = tyi * tileCountX + txi
            let rails = mazeGhostRails.[i]
            if (rails &&& MazeByteRight) <> 0uy then
                FacingRight
            else if (rails &&& MazeByteLeft) <> 0uy then
                FacingLeft
            else if (rails &&& MazeByteUp) <> 0uy then
                FacingUp
            else if (rails &&& MazeByteDown) <> 0uy then
                FacingDown
            else 
                failwith "Cannot determine initial direction for ghost because rails are not set on the initial tile"


/// The railsByte defines permissable directions, and we return
/// true if movement in the given direction is allowed by the railsByte.
let IsDirectionAllowedBy railsByte facingDirection =

    (railsByte &&& (facingDirection |> FacingDirectionToBitMaskByte)) <> 0uy


/// Obtain the *inner* collision rectangle given the top left
/// position of a tile-sized rectangle.
let CollisionRectangle position =
    let x = position.ptx
    let y = position.pty
    let border = (TileSide - CollisionSide) / 2
    {
        Left   = x + border
        Top    = y + border
        Right  = (x + TileSide) - border
        Bottom = (y + TileSide) - border
    }


/// Obtain the bounding rectangle of a tile sized
/// rectangle with its top left at a given pixel position.
let TileBoundingRectangle position =
    let x = position.ptx
    let y = position.pty
    {
        Left   = x
        Top    = y
        Right  = x + TileSide
        Bottom = y + TileSide
    }




/// Obtain pacman's collision rectangle.
let inline PacCollisionRectangle pacman =
    pacman.PacPosition |> CollisionRectangle


/// Update pacman's mode.
let WithPacMode mode pacman =
    {
        PacPosition = pacman.PacPosition
        PacState2 =
            {
                PacMode            = mode
                PacFacingDirection = pacman |> Facing
                LivesLeft          = pacman |> LivesLeft
                PacStartPosition   = pacman |> StartPosition
            }
    }

    
/// Obtain pacman's collision rectangle.
let inline GhostCollisionRectangle ghost =
    ghost.GhostPosition |> CollisionRectangle


/// Update the mode of the given ghost.
let WithGhostMode mode ghost =
    {
        GhostPosition = ghost.GhostPosition
        GhostState2   =
            {
                GhostMode             = mode
                GhostTag              = ghost |> Tag
                GhostBasePosition     = ghost |> BasePosition
                GhostInitialDirection = ghost |> InitialDirection
                GhostFacingDirection  = ghost |> GlideDirection
                GhostCornerProbTurn   = ghost |> TurnProb
                GhostThreeOrFourWayProbabilities = ghost |> Traits
            }
    }


/// Asks whether the given ghost is at its base home position.
let inline IsAtHomePosition ghost =
    ghost.GhostPosition = (ghost |> BasePosition)


/// Asks whether a pacman-maze-tile sized area overlaps any
/// of the GhostNormal-state ghosts in the list.  Ghosts in
/// other states never participate in intersection.
/// The tilePos denotes the top left corner.
let IntersectsNormalGhostsIn ghostStateList rect =

    ghostStateList 
        |> List.exists (fun ghost ->
            match ghost |> GhostMode with
                | GhostNormal -> ghost |> GhostCollisionRectangle |> RectangleIntersects rect
                | GhostEdibleUntil _
                | GhostRegeneratingUntil _
                | GhostReturningToBase -> false)



/// Wrapping function for pacman and ghosts for when 
/// passages exit the sides of the grid.
let private PointWrappedAtMazeEdges mazeState point =

    let { ptx=x ; pty=y } = point

    let width  = mazeState.MazeTilesCountX * TileSide
    let height = mazeState.MazeTilesCountY * TileSide

    let h = TileSide / 2

    let x = 
        if x < -h then x + width
        else if x >= (width-h)  then x - width
        else x

    let y = 
        if y < -h then y + height 
        else if y >= (height-h) then y - height
        else y

    { ptx=x ; pty=y }




// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  CORRIDOR DETERMINATION
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Returns the corridor rectangle starting from a given maze tile 'originTile',
/// where originTile is a 2D array index into the tiles matrix.  Takes the actual
/// maze, not the rails as the mazeByteArray.
let CorridorRectangle tilesHorizontally tilesVertically (mazeByteArray:byte[]) originTile direction =  // TODO: originTile's unit is not clear

    let stepDelta =
        direction |> DirectionToMovementDelta 0 1

    let isWall (t:byte) =
        t >= ((byte)TileIndex.Wall0) && t <= ((byte)TileIndex.Wall15)
    
    let hasNoExitInDirectionOfTravel pos =  // Not strictly correct, will cause inclusion of the wall square hit, but that is benign for our purposes.
        isWall (mazeByteArray.[pos.pty * tilesHorizontally + pos.ptx])

    let noSquareExistsAt pos =
        pos.ptx < 0 || pos.pty < 0 || pos.ptx >= tilesHorizontally || pos.pty >= tilesVertically

    let boundingRectangleOfSquareAt pos =
        pos |> PointMult TileSide |> TileBoundingRectangle

    let rec stepper  stepDelta position accumulator =
        
        let nextPosition = position |> PointMovedByDelta stepDelta

        if noSquareExistsAt nextPosition || position |> hasNoExitInDirectionOfTravel then
            accumulator
        else
            let r = boundingRectangleOfSquareAt nextPosition
            let union = TightestBoundingRectangleOf r accumulator
            stepper  stepDelta nextPosition union

    stepper stepDelta originTile (boundingRectangleOfSquareAt originTile)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  DRAWING
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private OriginForMazeOfDimensions cx cy (countX:int) (countY:int) =

    let x = cx - ((countX * TileSide) / 2)
    let y = cy - ((countY * TileSide) / 2)

    (x,y)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private DrawSpecificMazeCentred render tilesImage cx cy countX countY (mazeByteArray:byte[]) gameTime =

    let (x,y) = OriginForMazeOfDimensions cx cy countX countY

    for ty in 0..countY - 1 do
        let y' = y + ty * TileSide

        for tx in 0..countX - 1 do
            let tileIndex = mazeByteArray.[ty * countX + tx]
            let x' = x + tx * TileSide
            DrawPacTileInt render tilesImage x' y' ((int)tileIndex) gameTime

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private DrawMazeCentred render image cx cy mazeState gameTime =

    DrawSpecificMazeCentred 
        render image cx cy 
        mazeState.MazeTilesCountX
        mazeState.MazeTilesCountY
        mazeState.MazeTiles
        gameTime

    // To show the ghost rails:
    // DrawSpecificMazeCentred 
    //     render image cx cy 
    //     mazeState.MazeTilesCountX
    //     mazeState.MazeTilesCountY
    //     mazeState.MazeGhostRails
    //     gameTime

    // To show the player rails:
    // DrawSpecificMazeCentred 
    //     render image cx cy 
    //     mazeState.MazeTilesCountX
    //     mazeState.MazeTilesCountY
    //     mazeState.MazePlayersRails
    //     gameTime

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// A debugging facility to render rectangles returned by the CorridorRectangle function.
/// For debug purposes, pacman's position and direction are used as the reference point
/// and corridor search direction.
let private DrawCorridorFinderResult render centreX centreY countX countY mazeByteArray position facing =

    let (x,y) = OriginForMazeOfDimensions centreX centreY countX countY

    let pos    = { ptx=position.ptx / TileSide ; pty=position.pty / TileSide }
    let origin = { modx=x ; mody=y }
    let r      = CorridorRectangle countX countY mazeByteArray pos facing |> RectangleMovedByDelta origin

    let shape =
        DrawingShapes.DrawFilledRectangle (
            r.Left, r.Top, (r |> RectangleWidth), (r |> RectangleHeight), (DrawingShapes.SolidColour 0xFF00FFu))

    render shape

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderPacmanScreen render (model:PacmanScreenModel) gameTime =

    let backgroundImage = BackgroundImageID |> ImageFromID
    Image1to1 render 0<epx> 0<epx> backgroundImage

    let tilesImage = Level1ImageID |> ImageFromID

    let cx,cy = (ScreenWidthInt / 2) , (ScreenHeightInt / 2) 

    DrawMazeCentred 
        render tilesImage 
        cx cy
        model.MazeState
        gameTime

    // DrawCorridorFinderResult 
    //     render cx cy 
    //     model.MazeState.MazeTilesCountX 
    //     model.MazeState.MazeTilesCountY
    //     model.MazeState.MazeTiles
    //     model.PacmanState.PacPosition
    //     model.PacmanState |> Facing

    let (originx,originy) = 
        OriginForMazeOfDimensions 
            cx cy 
            model.MazeState.MazeTilesCountX 
            model.MazeState.MazeTilesCountY

    let pos = model.PacmanState.PacPosition 
                |> PointWrappedAtMazeEdges model.MazeState
                |> OffsetByOrigin originx originy 
    
    let direction = model.PacmanState |> Facing

    match model.PacmanState |> PacMode with
        | PacAlive ->
            let drawPacMode = if model.GhostsState |> InPillMode then DrawPacPillMode else DrawPacNormal
            DrawPacManAlive render tilesImage pos direction drawPacMode gameTime

        | PacDyingUntil _ ->
            if gameTime |> PulseActiveAtRate PacmanDyingFlashRate then
                DrawPacManAlive render tilesImage pos direction DrawPacZapped gameTime
            else
                () // No graphics desired.

        | PacDead ->
            () // No graphics desired.

    model.GhostsState
        |> List.iteri (fun i ghostState ->

            // DrawCorridorFinderResult 
            //     render cx cy 
            //     model.MazeState.MazeTilesCountX 
            //     model.MazeState.MazeTilesCountY
            //     model.MazeState.MazeTiles
            //     ghostState.GhostPosition
            //     ghostState |> GlideDirection

            let pos =
                ghostState.GhostPosition
                    |> PointWrappedAtMazeEdges model.MazeState
                    |> OffsetByOrigin originx originy
                        
            let number = ghostState |> Tag
            let mode   = ghostState |> GhostMode

            DrawGhost render tilesImage pos number mode gameTime)

    let indent = 20<epx>
    let indentY = 2<epx>

    Text render GreyFontID LeftAlign  TopAlign    indent indentY (sprintf "SCORE %d" model.ScoreAndHiScore.Score)  // TODO: memoize score to avoid garbage
    Text render GreyFontID RightAlign TopAlign    (ScreenWidthInt - indent) indentY (sprintf "HISCORE %d" model.ScoreAndHiScore.HiScore)  // TODO: memoize score to avoid garbage
    Text render GreyFontID LeftAlign  BottomAlign indent (ScreenHeightInt - indentY) (sprintf "FRAME %d" model.LevelNumber)  // TODO: memoize score to avoid garbage
    Text render GreyFontID RightAlign BottomAlign (ScreenWidthInt - indent) (ScreenHeightInt - indentY) (sprintf "LIVES %d" (model.PacmanState |> LivesLeft))  // TODO: memoize score to avoid garbage



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  PAC MAN himself:  State advance per frame
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

[<Struct>]
type PacHasEaten =
    | EatenNothing
    | EatenDot of dotTileArrayIndex:int
    | EatenPowerPill of powerPillTileArrayIndex:int

/// Handles all the positive things for pac man:  Moving and eating.
/// Returns an indicator of what happened.
let private AdvancePacMan keyStateGetter mazeState pacmanState =
    
    let position  = pacmanState.PacPosition
    let direction = pacmanState |> Facing
    let mode      = pacmanState |> PacMode

    match mode with

        | PacAlive ->
            
            let struct (up, down, left, right) = KeysFrom keyStateGetter
            
            let directionImpliedByKeys  = KeyStatesToDirection up down left right direction
            let angleToCurrentDirection = AngleBetween directionImpliedByKeys direction

            let tile = TileIndexOf position

            let direction =
                match angleToCurrentDirection with
                    | ZeroAngle 
                    | AboutTurn180 -> directionImpliedByKeys

                    | AntiClockwiseTurn90
                    | ClockwiseTurn90 ->
                        match tile with
                            | None -> direction // disallow, not perfectly aligned
                            | Some (txi, tyi) ->
                                let i = tyi * mazeState.MazeTilesCountX + txi
                                if directionImpliedByKeys 
                                    |> IsDirectionAllowedBy mazeState.MazePlayersRails.[i] then
                                    directionImpliedByKeys
                                else
                                    direction // disallow, no exit in that direction.

            let eaten, scoreIncrement =
                match tile with
                    | None -> EatenNothing , 0u
                    | Some (txi, tyi) ->
                        let i = tyi * mazeState.MazeTilesCountX + txi

                        let tileType = mazeState.MazeTiles.[i]
                        if tileType = ((byte) TileIndex.Dot) then
                            (EatenDot i) , ScoreForEatingDot

                        else if tileType = ((byte) TileIndex.Pill1) then   // We don't store Pill2 in the matrix.
                            (EatenPowerPill i) , ScoreForEatingPowerPill

                        else
                            EatenNothing , 0u

            let position =  // TODO: issue of frame rate!

                let proposedPosition = 
                    position 
                        |> PointMovedByDelta (direction |> DirectionToMovementDeltaI32)
                        |> PointWrappedAtMazeEdges mazeState

                match tile with
                    | None -> proposedPosition // Can always allow movement when inbetween tiles
                    | Some (txi, tyi) ->
                        let i = tyi * mazeState.MazeTilesCountX + txi
                        if direction |> IsDirectionAllowedBy mazeState.MazePlayersRails.[i] then
                            proposedPosition
                        else
                            position // disallow, no exit in that direction.
            
            (eaten , position , direction , scoreIncrement)            


        | PacDyingUntil _
        | PacDead ->
            (EatenNothing , position , direction , 0u)


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Algorithms
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

// TODO: Use PointWrappedAtMazeEdges when determining who sees who?   (Tiny edge case improvement).

let IsIntersectedByAnyOtherGhostTo selfGhost allGhosts corridorRect =

    allGhosts |> List.exists (fun otherGhost ->
        if selfGhost |> IsTheSameGhostAs otherGhost then
            false  // We do not "see" ourself down the corridors.
        else
            otherGhost.GhostPosition 
                |> TileBoundingRectangle
                |> RectangleIntersects corridorRect)



let IsDeadEndRail rail =

    System.Diagnostics.Debug.Assert (rail <> 0uy)   // Should never have empty rails.

    (rail = MazeByteLeft) || (rail = MazeByteRight) || (rail = MazeByteUp) || (rail = MazeByteDown)

let IsCornerRail rail =

    System.Diagnostics.Debug.Assert (rail <> 0uy)   // Should never have empty rails.

    let downRight = (MazeByteDown ||| MazeByteRight)
    let upRight   = (MazeByteUp   ||| MazeByteRight)
    let downLeft  = (MazeByteDown ||| MazeByteLeft)
    let upLeft    = (MazeByteUp   ||| MazeByteLeft)

    (rail = downRight) || (rail = upRight) || (rail = downLeft) || (rail = upLeft)

let IsStraightRail rail =

    System.Diagnostics.Debug.Assert (rail <> 0uy)   // Should never have empty rails.

    let horizontalRail = (MazeByteLeft ||| MazeByteRight)
    let verticalRail   = (MazeByteUp ||| MazeByteDown)

    (rail = horizontalRail)  ||  (rail = verticalRail)
    


let private EliminatingSuboptimalDirectionsForNormalGhost ghost mazeState tileXY pacRect allGhosts directionChoices =

    let corridorRect direction = 
        CorridorRectangle 
            mazeState.MazeTilesCountX
            mazeState.MazeTilesCountY
            mazeState.MazeTiles
            tileXY
            direction

    // TODO: We call corridorRect for 0-probability directions (which will never be chosen)
    //       and don't even read the rectangle.

    let corridorFacingLeft  = corridorRect FacingLeft
    let corridorFacingUp    = corridorRect FacingUp
    let corridorFacingRight = corridorRect FacingRight
    let corridorFacingDown  = corridorRect FacingDown

    let leftOnly  prob = { ProbLeft=prob ; ProbUp=0uy  ; ProbRight=0uy  ; ProbDown=0uy  }
    let upOnly    prob = { ProbLeft=0uy  ; ProbUp=prob ; ProbRight=0uy  ; ProbDown=0uy  }
    let rightOnly prob = { ProbLeft=0uy  ; ProbUp=0uy  ; ProbRight=prob ; ProbDown=0uy  }
    let downOnly  prob = { ProbLeft=0uy  ; ProbUp=0uy  ; ProbRight=0uy  ; ProbDown=prob }
    
    let lookForPacman prob corridorRect singleDirectionWithProbability acc =
        match acc with
            | Some _directionWherePacmanIsAlreadySeen -> acc
            | None ->
                if prob = 0uy then
                    None
                else if pacRect |> RectangleIntersects corridorRect then
                    Some (singleDirectionWithProbability prob)
                else
                    None

    let seenPac =
        None
            |> lookForPacman  directionChoices.ProbLeft   corridorFacingLeft   leftOnly  
            |> lookForPacman  directionChoices.ProbUp     corridorFacingUp     upOnly    
            |> lookForPacman  directionChoices.ProbRight  corridorFacingRight  rightOnly 
            |> lookForPacman  directionChoices.ProbDown   corridorFacingDown   downOnly  

    match seenPac with
        | Some directionTowardsPacman -> directionTowardsPacman
        | None ->

            let possiblyEliminated prob corridorRect direction =
                if prob = 0uy then
                    0uy
                else if corridorRect |> IsIntersectedByAnyOtherGhostTo ghost allGhosts then
                    0uy
                else
                    prob

            {
                ProbLeft  = possiblyEliminated  directionChoices.ProbLeft   corridorFacingLeft   FacingLeft
                ProbUp    = possiblyEliminated  directionChoices.ProbUp     corridorFacingUp     FacingUp
                ProbRight = possiblyEliminated  directionChoices.ProbRight  corridorFacingRight  FacingRight
                ProbDown  = possiblyEliminated  directionChoices.ProbDown   corridorFacingDown   FacingDown
            }



let private EliminatingSuboptimalDirectionsForEdibleGhost mazeState tileXY pacRect directionChoices =

    let possiblyEliminated probability direction =
        if probability = 0uy then
            0uy
        else
            let corridorRect = 
                CorridorRectangle 
                    mazeState.MazeTilesCountX
                    mazeState.MazeTilesCountY
                    mazeState.MazeTiles
                    tileXY
                    direction

            if corridorRect |> RectangleIntersects pacRect then
                0uy
            else
                probability

    {
        ProbLeft  = possiblyEliminated  directionChoices.ProbLeft   FacingLeft
        ProbUp    = possiblyEliminated  directionChoices.ProbUp     FacingUp
        ProbRight = possiblyEliminated  directionChoices.ProbRight  FacingRight
        ProbDown  = possiblyEliminated  directionChoices.ProbDown   FacingDown
    }



/// Returns true if no directions are available according to 
/// individual direction probabilities.
let NoDirectionsAvailable directionChoices = 
    (directionChoices.ProbLeft 
        ||| directionChoices.ProbUp 
        ||| directionChoices.ProbDown 
        ||| directionChoices.ProbRight) = 0uy



/// Returns a direction chosen from those which have non-zero probability.
/// The choice is pseudo-random, weighted on the probability values of the
/// direction choices.
let DirectionChosenRandomlyFrom directionChoices (XorShift32State(rand)) =

    let l = ((uint32) directionChoices.ProbLeft)
    let u = ((uint32) directionChoices.ProbUp)
    let d = ((uint32) directionChoices.ProbDown)
    let r = ((uint32) directionChoices.ProbRight)

    let probTotal = l + u + r + d

    System.Diagnostics.Debug.Assert (probTotal <> 0u)  // There must always be one direction.

    let n = rand % probTotal

    if l <> 0u && n < l then FacingLeft
    else if u <> 0u && n < (l + u) then FacingUp
    else if d <> 0u && n < (l + u + d) then FacingDown
    else if r <> 0u && n < (l + u + d + r) then FacingRight
    else failwith "It should never be the case there are *no* exits from a decision point!"



    
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Ghost position advance
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let EliminatingDirectionChoicesGivenByBitmask bitmaskByte directionChoices =

    System.Diagnostics.Debug.Assert (bitmaskByte <> 0uy)   // Should never have empty directions mask.

    let maskedBy mask probValue =
        if mask=0uy then 0uy else probValue

    {
        ProbLeft  = directionChoices.ProbLeft  |> maskedBy (bitmaskByte &&& MazeByteLeft )
        ProbUp    = directionChoices.ProbUp    |> maskedBy (bitmaskByte &&& MazeByteUp   )
        ProbRight = directionChoices.ProbRight |> maskedBy (bitmaskByte &&& MazeByteRight)
        ProbDown  = directionChoices.ProbDown  |> maskedBy (bitmaskByte &&& MazeByteDown )
    }


let private DecideNewPositionAndDirectionFor 
    (ghost:GhostState) 
    mazeState 
    (allGhosts:GhostState list) 
    (pacman:PacmanState) 
    (rand:XorShift32State)
    gameTime =

    // NB:  Only called for GhostNormal and GhostEdibleUntil cases.

    let position  = ghost.GhostPosition
    let direction = ghost |> GlideDirection
    let tile      = TileIndexOf position
    let rails     = mazeState.MazeGhostRails

    let direction =
        match tile with

            | None -> 
                direction

            | Some (txi, tyi) ->  // Ghost precisely on the tile at (txi,tyi).  This is a decision point.
                let i = tyi * mazeState.MazeTilesCountX + txi
                let railsBitmask = rails.[i]
                let tileXY = { ptx=txi ; pty=tyi }

                if railsBitmask |> IsStraightRail then
                    direction

                else if railsBitmask |> IsDeadEndRail then
                    railsBitmask |> SingleBitInByteToFacingDirection

                else if railsBitmask |> IsCornerRail then
                    let (XorShift32State(r)) = rand
                    let p = (byte) (r % 100u)
                    if p < (ghost |> TurnProb) then
                        direction |> TurnCorner railsBitmask
                    else
                        direction |> ReverseCornerDir railsBitmask

                else
                    let defaultDirectionChoices = 
                        ghost 
                            |> GhostDirectionChoiceProbabilities 
                            |> EliminatingDirectionChoicesGivenByBitmask railsBitmask

                    let directionChoices =

                        let pacRect = pacman.PacPosition |> TileBoundingRectangle

                        match ghost |> GhostMode with
                            
                            | GhostNormal -> 
                                defaultDirectionChoices 
                                    |> EliminatingSuboptimalDirectionsForNormalGhost ghost mazeState tileXY pacRect allGhosts
                            
                            | GhostEdibleUntil _ -> 
                                defaultDirectionChoices 
                                    |> EliminatingSuboptimalDirectionsForEdibleGhost mazeState tileXY pacRect
                            
                            | _ -> failwith "Should not be deciding direction for ghost in this state"
                        
                        |> UpdateToValueWhen NoDirectionsAvailable defaultDirectionChoices

                    DirectionChosenRandomlyFrom directionChoices rand

    let position = 

        let potential =
            position 
                |> PointMovedByDelta (direction |> DirectionToMovementDeltaI32)
                |> PointWrappedAtMazeEdges mazeState

        match ghost |> GhostMode with
            | GhostNormal -> potential
            | GhostEdibleUntil _ ->
                if gameTime |> PulseActiveAtRate 20.0F then potential else position  // TODO: better treatment.
            | _ -> failwith "Should not be deciding direction for ghost in this state"

    (position, direction)



let MovedTowardsHomePosition ghost =

    let delta = 
        ghost.GhostPosition 
            |> SimpleMovementDeltaI32ToGetTo (ghost |> BasePosition)

    let position = 
        ghost.GhostPosition 
            |> PointMovedByDelta delta

    (position , ghost |> GlideDirection)



let private AdvanceGhost mazeState allGhosts pacman ghost rand gameTime =

    let (position , direction) =
        match ghost |> GhostMode with
            | GhostNormal
            | GhostEdibleUntil _ -> 
                DecideNewPositionAndDirectionFor ghost mazeState allGhosts pacman rand gameTime

            | GhostReturningToBase ->
                ghost |> MovedTowardsHomePosition

            | GhostRegeneratingUntil _ ->
                // No movement while re-generating in the base.
                (ghost.GhostPosition , ghost |> GlideDirection) 

    {
        GhostPosition = position
        GhostState2 =
            {
                GhostTag              = ghost |> Tag
                GhostBasePosition     = ghost |> BasePosition
                GhostMode             = ghost |> GhostMode
                GhostCornerProbTurn   = ghost |> TurnProb
                GhostInitialDirection = ghost |> InitialDirection
                GhostFacingDirection  = direction
                GhostThreeOrFourWayProbabilities = ghost |> Traits
            }
    }


let private WithGhostMovement mazeState pacman rand gameTime allGhosts =

    let mutable rand = rand  // TODO: remove

    allGhosts |> List.map (fun ghost -> 
        rand <- rand |> XorShift32
        AdvanceGhost mazeState allGhosts pacman ghost rand gameTime)


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Post Life Loss Handling
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Return pacmans position reset, for use after life loss.
let WithPacmanReset pacmanState =
    // TODO: If LivesLeft = 1 on entry then this should return None
    { 
        PacPosition = pacmanState |> StartPosition
        PacState2 =
            { 
                PacFacingDirection = FacingRight
                PacMode            = PacAlive
                LivesLeft          = (pacmanState |> LivesLeft) - 1
                PacStartPosition   = pacmanState |> StartPosition
            } 
    }


/// Return ghosts position reset, for use after pacman life loss.
let WithGhostReset ghostState =
    { 
        GhostPosition = ghostState |> BasePosition
        GhostState2 = 
            { 
                GhostMode             = GhostNormal
                GhostInitialDirection = ghostState |> InitialDirection
                GhostFacingDirection  = ghostState |> InitialDirection
                GhostTag              = ghostState |> Tag
                GhostBasePosition     = ghostState |> BasePosition
                GhostCornerProbTurn   = ghostState |> TurnProb
                GhostThreeOrFourWayProbabilities = ghostState |> Traits
            } 
    }


/// Returns a model with the characters reset for use after a life loss.
let private WithCharactersReset model =
    {
        Random            = model.Random
        LevelNumber       = model.LevelNumber
        ScoreAndHiScore   = model.ScoreAndHiScore  
        MazeState         = model.MazeState        
        PacmanState       = model.PacmanState |> WithPacmanReset
        GhostsState       = model.GhostsState |> List.map WithGhostReset
        WhereToOnGameOver = model.WhereToOnGameOver
        WhereToOnAllEaten = model.WhereToOnAllEaten
    }





// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Collisions PAC vs GHOSTS
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// State changes on pacman as a result of collision detection with ghosts
let WithStateChangesResultingFromCollisionWithGhosts ghostStateList gameTime pacmanState =   // TODO: Return indicator of new state, instead of new record

    match pacmanState |> PacMode with

        | PacAlive ->
            let pacmanRectangle = pacmanState |> PacCollisionRectangle
            pacmanState |> UpdateIf 
                (pacmanRectangle |> IntersectsNormalGhostsIn ghostStateList)
                (WithPacMode (PacDyingUntil (gameTime + PacmanDyingAnimationTime)))

        | PacDyingUntil t ->
            pacmanState |> UpdateIf (gameTime >= t) (WithPacMode PacDead)

        | PacDead -> 
            pacmanState



/// State changes on ghosts as a result of collision detection with pacman
let WithStateChangesResultingFromCollisionWithPacman pacmanPos ghosts =   // TODO: Return indicator of new state, instead of new record

    let pacmanRectangle = pacmanPos |> PacCollisionRectangle

    let isEdibleGhostOverlappingPacmanAt pacmanRectangle ghost =

        match ghost |> GhostMode with
            | GhostNormal
            | GhostReturningToBase      
            | GhostRegeneratingUntil _  -> false
            | GhostEdibleUntil _ -> 
                ghost 
                    |> GhostCollisionRectangle
                    |> RectangleIntersects pacmanRectangle

    // Garbage optimisation:  Scan for intersections and calculate score for eaten ghosts.

    let score = 
        ghosts |> List.sumBy (fun ghost ->
            if ghost |> isEdibleGhostOverlappingPacmanAt pacmanRectangle then 
                ScoreForEatingGhost 
            else 
                NoScore)

    let ghosts = 
        ghosts |> 
            UpdateIf 
                (score > 0u)
                (List.map (UpdateWhen 
                                (isEdibleGhostOverlappingPacmanAt pacmanRectangle) 
                                (WithGhostMode GhostReturningToBase)))

    struct (ghosts , score)


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  State update application
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// State update for pacman position and direction.
let WithPacManMovementStateChangesAppliedFrom position direction pacmanState =

    if direction = (pacmanState |> Facing) then
        {
            PacState2 = pacmanState.PacState2  // unchanged
            PacPosition = position
        }
    else
        {
            PacState2 =
                {
                    PacMode            = pacmanState |> PacMode
                    PacFacingDirection = direction
                    LivesLeft          = pacmanState |> LivesLeft
                    PacStartPosition   = pacmanState |> StartPosition
                }
            PacPosition = position
        }


/// State update for the maze as pac man eats things.
let private WithDotsRemovedFromArrayWhere eaten mazeState =

    match eaten with
        | EatenNothing -> mazeState
        | EatenDot tileIndex
        | EatenPowerPill tileIndex ->
            mazeState.MazeTiles.[tileIndex] <- ((byte) TileIndex.Blank)  // mutable
            mazeState


/// State update for ghosts if pacman eats a power pill
let private WithEdibleGhostsIfPowerPill eaten gameTime ghostStateList =

    match eaten with
        | EatenNothing 
        | EatenDot _ -> ghostStateList
        | EatenPowerPill _ ->
            ghostStateList |> List.map (fun ghost ->
                match ghost |> GhostMode with
                    | GhostNormal
                    | GhostEdibleUntil _ -> ghost |> WithGhostMode (GhostEdibleUntil (gameTime + PowerPillTime))
                    | GhostReturningToBase
                    | GhostRegeneratingUntil _ -> ghost  // This ghost does not become edible.
            )


/// State update for ghosts based on timeouts.
let private WithReturnToNormalityIfTimeOutAt gameTime ghostStateList =
    ghostStateList |> List.map (fun ghost ->
        match ghost |> GhostMode with
            | GhostNormal ->
                ghost

            | GhostEdibleUntil t
            | GhostRegeneratingUntil t -> 
                ghost |> UpdateIf (gameTime >= t) (WithGhostMode GhostNormal)

            | GhostReturningToBase ->
                ghost |> UpdateWhen IsAtHomePosition (WithGhostMode (GhostRegeneratingUntil (gameTime + RegenerationTime)))
    )





// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Screen state advance on frame
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextPacmanScreenState gameState keyStateGetter gameTime elapsed =

    // Unpack

    let model = ModelFrom gameState

    let {
            Random            = rand
            ScoreAndHiScore   = scoreAndHiScore
            MazeState         = mazeState
            PacmanState       = pacmanState
            GhostsState       = ghostStateList
            WhereToOnGameOver = _
            WhereToOnAllEaten = _
        }
            = model

    let eaten , position , direction , scoreIncrement =
        AdvancePacMan keyStateGetter mazeState pacmanState

    let scoreAndHiScore =
        scoreAndHiScore |> ScoreIncrementedBy scoreIncrement

    let pacmanState =
        pacmanState |> WithPacManMovementStateChangesAppliedFrom position direction

    let mazeState =
        mazeState |> WithDotsRemovedFromArrayWhere eaten

    let ghostStateList =
        ghostStateList 
            |> WithReturnToNormalityIfTimeOutAt gameTime
            |> WithEdibleGhostsIfPowerPill eaten gameTime
            |> WithGhostMovement mazeState pacmanState rand gameTime

    let pacmanState =
        pacmanState |> WithStateChangesResultingFromCollisionWithGhosts ghostStateList gameTime

    let struct (ghostStateList , scoreIncrement) =
        ghostStateList |> WithStateChangesResultingFromCollisionWithPacman pacmanState

    let scoreAndHiScore =
        scoreAndHiScore |> ScoreIncrementedBy scoreIncrement

    // Repack

    let model =
        {
            Random            = model.Random |> XorShift32
            LevelNumber       = model.LevelNumber
            ScoreAndHiScore   = scoreAndHiScore
            MazeState         = mazeState
            PacmanState       = pacmanState
            GhostsState       = ghostStateList
            WhereToOnGameOver = model.WhereToOnGameOver
            WhereToOnAllEaten = model.WhereToOnAllEaten
        }        

    // Decide next gameState

    if pacmanState |> LifeIsOver then
        if pacmanState |> GameIsOver then
            model.WhereToOnGameOver scoreAndHiScore
        else
            let whereToAfterIntermission =
                fun _gameTime -> model |> WithCharactersReset |> ReplacesModelIn gameState
            WithLifeLossIntermissionCard whereToAfterIntermission gameTime  // TODO: There is something bad about this parameter order, that we can't use |>
    
    else if mazeState |> IsAllEaten then
        model.WhereToOnAllEaten model.LevelNumber scoreAndHiScore gameTime  // TODO: Maze flash - but could that be done with a clever external filter?
    
    else 
        gameState |> WithUpdatedModel model

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewPacmanScreen levelNumber whereToOnAllEaten whereToOnGameOver scoreAndHiScore =

    let numberOfMazes = 
        AllPacmanMazes.Length

    let unpackedMaze = 
        AllPacmanMazes.[levelNumber % numberOfMazes] |> TextMazeDefinitionUnpacked

    let ghostMovementTraitsArray =
        [|
            // Blue
            {
                CornerProbTurn = 85uy
                ProbAhead   = 50uy
                ProbTurn90  = 20uy
                ProbTurn180 = 30uy
            }
            // Green
            {
                CornerProbTurn = 85uy
                ProbAhead   = 80uy
                ProbTurn90  = 15uy
                ProbTurn180 =  5uy
            }
            // Pink
            {
                CornerProbTurn = 85uy
                ProbAhead   = 80uy
                ProbTurn90  = 15uy
                ProbTurn180 =  5uy
            }
            // Red
            {
                CornerProbTurn = 100uy
                ProbAhead   = 50uy
                ProbTurn90  = 50uy
                ProbTurn180 =  0uy
            }
        |]

    let screenModel =
        {
            Random            = XorShift32State ScreenRandomSeed
            LevelNumber       = levelNumber
            ScoreAndHiScore   = scoreAndHiScore
            MazeState         = unpackedMaze.UnpackedMazeState
            WhereToOnGameOver = whereToOnGameOver
            WhereToOnAllEaten = whereToOnAllEaten
            
            PacmanState =
                { 
                    PacPosition = unpackedMaze.UnpackedPacmanPosition
                    PacState2 = 
                        { 
                            PacFacingDirection = FacingRight
                            PacMode = PacAlive
                            LivesLeft = InitialLives
                            PacStartPosition = unpackedMaze.UnpackedPacmanPosition
                        } 
                }

            GhostsState = unpackedMaze.UnpackedGhostPositions |> List.mapi (fun i ghostPos -> 
                
                let n = i % ghostMovementTraitsArray.Length

                let facing = 
                    InitialFacingDirectionFor 
                        ghostPos 
                        unpackedMaze.UnpackedMazeState.MazeGhostRails 
                        unpackedMaze.UnpackedMazeState.MazeTilesCountX

                { 
                    GhostPosition = ghostPos

                    GhostState2 = 
                        { 
                            GhostTag              = GhostNumber(i)
                            GhostMode             = GhostNormal
                            GhostBasePosition     = ghostPos
                            GhostInitialDirection = facing
                            GhostFacingDirection  = facing
                           
                            GhostCornerProbTurn = 
                                ghostMovementTraitsArray.[n].CornerProbTurn

                            GhostThreeOrFourWayProbabilities = 
                                ghostMovementTraitsArray.[n] 
                                    |> CalculateMemoizedDirectionProbabilities
                        } 
                })
        }

    NewGameState NextPacmanScreenState RenderPacmanScreen screenModel

