module ScreenPacman

open Rules
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
open Mazes
open MazeState
open ScreenIntermissions
open Random
open Directions
open GhostAI
open Update
open Keys
open MazeUnpacker


let ScreenRandomSeed = 0x33033u

// TODO: BUG: Frame 145:  If pacman stays still, the ghosts don't get him coming from the top.

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
        WhereToOnAllEaten      : int -> BetweenScreenStatus -> float32<seconds> -> ErasedGameState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Support functions
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Expansion of pacman's bounding rectangle to allow ghosts to still
/// see pacman if he slips around a corner.  (Ghosts have no memory).
let ExpandedToSlightlyCompensateForLackOfGhostMemory = 
    InflateRectangle TileSide

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
                GhostFacingDirection  = ghost |> GlideDirection
                GhostAITable          = AIFor ghost
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

/// Return a zero thickness rectangle that lies along the edge
/// of the given rectangle at the side indicated by the direction.
let EdgeRectangleFacing direction rectangle =
    match direction with
        | FacingLeft   -> { rectangle with Right  = rectangle.Left   }
        | FacingUp     -> { rectangle with Bottom = rectangle.Top    }
        | FacingRight  -> { rectangle with Left   = rectangle.Right  }
        | FacingDown   -> { rectangle with Top    = rectangle.Bottom }



/// Returns the corridor rectangle starting from a given maze tile 'originTile',
/// where originTile is a 2D array index into the tiles matrix.  Takes the actual
/// maze, not the rails as the mazeByteArray.
let CorridorRectangle tilesHorizontally tilesVertically (mazeByteArray:byte[]) originTile direction =  // TODO: originTile's unit is not clear (it's the array indices)

    let stepDelta =
        direction |> DirectionToMovementDelta 0 1

    let isWallTileType (t:byte) =
        t >= ((byte)TileIndex.Wall0) && t <= ((byte)TileIndex.Wall15)
    
    let isWall pos =  // Not strictly correct, will cause inclusion of the wall square hit, but that is benign for our purposes.
        isWallTileType (mazeByteArray.[pos.pty * tilesHorizontally + pos.ptx])

    let noSquareExistsAt pos =
        pos.ptx < 0 || pos.pty < 0 || pos.ptx >= tilesHorizontally || pos.pty >= tilesVertically

    let boundingRectangleOfSquareAt pos =
        pos |> PointMult TileSide |> TileBoundingRectangle

    let rec stepper  stepDelta position accumulator =
        
        let nextPosition = position |> PointMovedByDelta stepDelta

        if noSquareExistsAt nextPosition || nextPosition |> isWall then
            accumulator
        else
            let r = boundingRectangleOfSquareAt nextPosition
            let union = TightestBoundingRectangleOf r accumulator
            stepper  stepDelta nextPosition union

    let initialRectangle = (boundingRectangleOfSquareAt originTile) |> EdgeRectangleFacing direction
    stepper stepDelta originTile initialRectangle

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
let private DrawCorridorFinderResult render centreX centreY countX countY mazeByteArray position facing colour =

    let (x,y) = OriginForMazeOfDimensions centreX centreY countX countY

    let pos    = { ptx=position.ptx / TileSide ; pty=position.pty / TileSide }
    let origin = { modx=x ; mody=y }
    let r      = CorridorRectangle countX countY mazeByteArray pos facing |> RectangleMovedByDelta origin

    let shape =
        DrawingShapes.DrawFilledRectangle (
            r.Left, r.Top, (r |> RectangleWidth), (r |> RectangleHeight), colour)

    render shape

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private DrawBoundingRectangle render fillColour pacPosition =

    let pacRect = 
        pacPosition |> TileBoundingRectangle |> ExpandedToSlightlyCompensateForLackOfGhostMemory

    render
        (DrawingShapes.DrawFilledRectangle (
            pacRect.Left, pacRect.Top, (pacRect |> RectangleWidth), (pacRect |> RectangleHeight), fillColour))

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderPacmanScreen render (model:PacmanScreenModel) gameTime =

    let backgroundImage = BackgroundImageID |> ImageFromID
    Image1to1 render 0<epx> 0<epx> backgroundImage

    let tilesImage = Level1ImageID |> ImageFromID

    let cx,cy = (ScreenWidthInt / 2) , (ScreenHeightInt / 2) 

    let (originx,originy) = 
        OriginForMazeOfDimensions 
            cx cy 
            model.MazeState.MazeTilesCountX 
            model.MazeState.MazeTilesCountY

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
    //     (DrawingShapes.SolidColour 0xFF00FFu)

    let pos = model.PacmanState.PacPosition 
                |> PointWrappedAtMazeEdges model.MazeState
                |> OffsetByOrigin originx originy 
    
    let direction = model.PacmanState |> Facing

    // DrawBoundingRectangle render (DrawingShapes.SolidColour 0xCC0000u) pos

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

            let pos =
                ghostState.GhostPosition
                    |> PointWrappedAtMazeEdges model.MazeState
                    |> OffsetByOrigin originx originy
            
            // let (GhostNumber(gn)) = ghostState |> Tag
            // let colour = DrawingShapes.SolidColour ([| 0xFF0000u ; 0xFFFF00u ; 0x00FFFFu ; 0xFFFFFFu |].[gn])
            // // DrawBoundingRectangle render colour pos
            // DrawCorridorFinderResult 
            //     render cx cy 
            //     model.MazeState.MazeTilesCountX 
            //     model.MazeState.MazeTilesCountY
            //     model.MazeState.MazeTiles
            //     ghostState.GhostPosition
            //     (ghostState |> GlideDirection)
            //     colour

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



let HasMoreThanOnePossibleDirection directionProbs =
    let test prob = if prob > 0uy then 1 else 0
    let l = test directionProbs.ProbLeft
    let u = test directionProbs.ProbUp
    let r = test directionProbs.ProbRight
    let d = test directionProbs.ProbDown
    (l + u + r + d) > 1


/// Adjust the probability values in the input directionChoices
/// with regard to environmental factors that surround this 'normal'
/// mode ghost.
let private WithAdjustmentsForNormalGhost 
    ghost mazeState tileXY pacPos allGhosts directionChoices =

    let fatPacRect = 
        pacPos |> TileBoundingRectangle |> ExpandedToSlightlyCompensateForLackOfGhostMemory

    let corridorRectInDirection direction = 
        CorridorRectangle 
            mazeState.MazeTilesCountX
            mazeState.MazeTilesCountY
            mazeState.MazeTiles
            tileXY
            direction

    let consider facingDirection 
        (probsForSingleDirection:DirectionChoiceProbabilities) 
        (exceptFacingDirection:DirectionChoiceProbabilities -> DirectionChoiceProbabilities) 
        (directionsAcc:DirectionChoiceProbabilities) =
        
        if  (directionsAcc |> HasMoreThanOnePossibleDirection)  &&
            (directionsAcc |> ProbOfDirection facingDirection) <> 0uy then
            
            let corridorRectangle = 
                corridorRectInDirection facingDirection
            
            if fatPacRect |> RectangleIntersects corridorRectangle then
                probsForSingleDirection  // chase pacman
            
            else if corridorRectangle |> IsIntersectedByAnyOtherGhostTo ghost allGhosts then
                directionsAcc |> exceptFacingDirection   // avoid buddy
            
            else
                directionsAcc
        else
            directionsAcc

    directionChoices
        |> consider FacingLeft  LeftOnly  ExceptLeft
        |> consider FacingUp    UpOnly    ExceptUp
        |> consider FacingRight RightOnly ExceptRight
        |> consider FacingDown  DownOnly  ExceptDown



/// Adjust the probability values in the input directionChoices
/// with regard to environmental factors that surround this 'edible'
/// mode ghost.
let private WithAdjustmentsForEdibleGhost 
    mazeState tileXY pacPos directionChoices =

    let pacRect = pacPos |> TileBoundingRectangle

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
                0uy  // run away
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

                if railsBitmask <> MazeByteCentralDotIndex then

                    let defaultDirectionChoices =
                        (AIFor ghost) |> GetDirectionProbabilities direction railsBitmask

                    let directionChoices = 

                        let pacPos = pacman.PacPosition
                        let tileXY = { ptx=txi ; pty=tyi }
                         
                        match ghost |> GhostMode with
                            
                            | GhostNormal -> 
                                defaultDirectionChoices 
                                    |> WithAdjustmentsForNormalGhost ghost mazeState tileXY pacPos allGhosts
                            
                            | GhostEdibleUntil _ -> 
                                defaultDirectionChoices 
                                    |> WithAdjustmentsForEdibleGhost mazeState tileXY pacPos
                            
                            | _ -> failwith "Should not be deciding direction for ghost in this state"
                        
                        |> UpdateToValueWhen NoDirectionsAvailable defaultDirectionChoices

                    let newDirection = DirectionChosenRandomlyFrom directionChoices rand
                
                    let mask = newDirection |> FacingDirectionToBitMaskByte
                    assert ((mask &&& railsBitmask) <> 0uy)  // This decision function should never decide a direction inconsistent with the rails.

                    newDirection

                else
                    direction  // arbitrary anyway, since we're trapped.

    let position = 

        let areTrapped =
            match tile with
                | None -> false  // because we're between tiles
                | Some (txi, tyi) ->  // Ghost precisely on the tile at (txi,tyi).  This is a decision point.
                    let i = tyi * mazeState.MazeTilesCountX + txi
                    let railsBitmask = rails.[i]
                    railsBitmask = MazeByteCentralDotIndex

        if not areTrapped then
            position 
                |> PointMovedByDelta (direction |> DirectionToMovementDeltaI32)
                |> PointWrappedAtMazeEdges mazeState
        else
            position


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
        (ghost.GhostPosition , ghost |> GlideDirection)

    let (position , direction) =
        match ghost |> GhostMode with
            | GhostNormal ->
                DecideNewPositionAndDirectionFor ghost mazeState allGhosts pacman rand gameTime

            | GhostEdibleUntil _ -> 
                if gameTime |> PulseActiveAtRate 20.0F then
                    DecideNewPositionAndDirectionFor ghost mazeState allGhosts pacman rand gameTime
                else
                    (position , direction)

            | GhostReturningToBase ->
                ghost |> MovedTowardsHomePosition

            | GhostRegeneratingUntil _ ->
                // No movement while re-generating in the base.
                (position , direction)

    {
        GhostPosition = position
        GhostState2 =
            {
                GhostTag              = ghost |> Tag
                GhostBasePosition     = ghost |> BasePosition
                GhostMode             = ghost |> GhostMode
                GhostFacingDirection  = direction
                GhostAITable          = AIFor ghost
            }
    }


let private WithGhostMovement mazeState pacman rand gameTime allGhosts =

    let mutable rand = rand  // TODO: remove

    allGhosts |> List.map (fun ghost -> 
        rand <- rand |> XorShift32
        AdvanceGhost mazeState allGhosts pacman ghost rand gameTime)  // TODO: Consider: the new position of the ghost doesn't get taken into account when processing the rest.  They still see the old positions.


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
                LivesLeft          = (pacmanState |> LivesLeft) - 1u
                PacStartPosition   = pacmanState |> StartPosition
            } 
    }


/// Return ghosts position reset, for use after pacman life loss.
let WithGhostReset ghost =
    { 
        GhostPosition = ghost |> BasePosition
        GhostState2 = 
            { 
                GhostMode             = GhostNormal
                GhostFacingDirection  = FacingUp  // Arbitrary choice.  Since we're directly over a tile, the direction choose will correct this.
                GhostTag              = ghost |> Tag
                GhostBasePosition     = ghost |> BasePosition
                GhostAITable          = AIFor ghost
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


/// State changes on pacman because of (possible) score increment
let WithStateChangesResultingFromNewScore scoreAndHiScore scoreIncrement pacmanState =
    
    if scoreIncrement > 0u then
        let newScore = scoreAndHiScore.Score
        let oldScore = newScore - scoreIncrement
        let a = oldScore / ScoreDeltaForExtraLife
        let b = newScore / ScoreDeltaForExtraLife
        if b > a then
            {
                pacmanState with 
                    PacState2 = { pacmanState.PacState2 with LivesLeft = pacmanState.PacState2.LivesLeft + 1u }
            }
        else
            pacmanState
    else
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

    let eaten , position , direction , scoreIncrement1 =
        AdvancePacMan keyStateGetter mazeState pacmanState

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

    let struct (ghostStateList , scoreIncrement2) =
        ghostStateList |> WithStateChangesResultingFromCollisionWithPacman pacmanState

    let scoreIncrement =
        scoreIncrement1 + scoreIncrement2

    let scoreAndHiScore =
        scoreAndHiScore |> ScoreIncrementedBy scoreIncrement

    let pacmanState =
        pacmanState |> WithStateChangesResultingFromNewScore scoreAndHiScore scoreIncrement

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
        let betweenScreenStatus = 
            {
                ScoreAndHiScore = model.ScoreAndHiScore
                Lives           = model.PacmanState.PacState2.LivesLeft
            }
        model.WhereToOnAllEaten model.LevelNumber betweenScreenStatus gameTime  // TODO: Maze flash - but could that be done with a clever external filter?
    
    else 
        gameState |> WithUpdatedModel model

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewPacmanScreen levelNumber whereToOnAllEaten whereToOnGameOver (betweenScreenStatus:BetweenScreenStatus) =

    let numberOfMazes = 
        AllPacmanMazes.Length

    let unpackedMaze = 
        AllPacmanMazes.[levelNumber % numberOfMazes] |> TextMazeDefinitionUnpacked

    let (charger, standard, ditherer) = GetGhostMoveTraits ()

    let ghostMovementTraitsArray =
        [|
            standard  // Blue
            ditherer  // Green
            standard  // Pink
            charger   // Red
        |]

    let screenModel =
        {
            Random            = XorShift32State ScreenRandomSeed
            LevelNumber       = levelNumber
            ScoreAndHiScore   = betweenScreenStatus.ScoreAndHiScore
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
                            LivesLeft = betweenScreenStatus.Lives
                            PacStartPosition = unpackedMaze.UnpackedPacmanPosition
                        } 
                }

            GhostsState = unpackedMaze.UnpackedGhostPositions |> List.mapi (fun i ghostPos -> 
                
                let ghostMovementTraits = 
                    ghostMovementTraitsArray.[i % ghostMovementTraitsArray.Length]

                { 
                    GhostPosition = ghostPos

                    GhostState2 = 
                        { 
                            GhostTag              = GhostNumber(i)
                            GhostMode             = GhostNormal
                            GhostBasePosition     = ghostPos
                            GhostFacingDirection  = FacingUp  // Arbitrary choice.  Since we're directly over a tile, the direction chooser can correct this.
                            GhostAITable          = ghostMovementTraits |> GhostMovementTable
                        } 
                })
        }

    NewGameState NextPacmanScreenState RenderPacmanScreen screenModel

