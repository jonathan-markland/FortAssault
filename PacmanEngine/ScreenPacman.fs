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

// TODO: gameState |> WithFreezeFrameFor PauseDuration gameTime whereToAfter


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private PauseDuration = 2.0F<seconds>

let private DefaultMaze =
    [|
        "#########.########.#"
        "#@..##......##.....#"
        "#.#....####....#.#.#"
        "..#.##########...#.#"
        "#.#............#.#.#"
        "#...###.####.#.#@..#"
        "#.#.#...#12:.#.#####"
        "..#...#.#34:.#......"
        "#...###0####.#.#.#.#"
        "###.#..........#.#.#"
        "#...#.########.#.#.#"
        "#.#.#........#.....#"
        "#@#.#.#.####.#.#@#.#"
        "#.....#........#...."
        "###.#####.##########"
    |]

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private MazeState =
    {
        MazeTilesCountX  : int
        MazeTilesCountY  : int
        MazeTiles        : byte[]  // content mutated by pacman eating things   // TODO: strong type wrapper for bytes
        MazeGhostRails   : byte[]  // not mutated
        MazePlayersRails : byte[]  // not mutated
    }

type private PacmanScreenModel =
    {
        ScoreAndHiScore        : ScoreAndHiScore
        MazeState              : MazeState
        PacmanState            : PacmanState
        GhostsState            : GhostState list
        WhereToOnGameOver      : ScoreAndHiScore -> ErasedGameState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type FindResult = InvalidMaze | NotFound | Found of int * int | NotUnique

let FindUnique charToFind (mazeArray:string[]) =

    let result =
        mazeArray |> IfValidStringRectangleThen (fun width height ->

            let mutable findResult = NotFound

            for y in 0..(height-1) do
                for x in 0..(width-1) do
                    if mazeArray.[y].[x] = charToFind then
                        findResult <-
                            match findResult with
                                | NotFound    -> Found(x,y)
                                | Found _     -> NotUnique
                                | NotUnique   -> NotUnique
                                | InvalidMaze -> failwith "unexpected case"  // should never happen
            Some findResult
        )

    match result with
        | Some findResult -> findResult
        | None -> InvalidMaze

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private UnpackedMaze =
    {
        UnpackedMazeState      : MazeState
        UnpackedPacmanPosition : Point<int<epx>>
        UnpackedGhostPositions : Point<int<epx>> list
    }

let private TextMazeDefinitionUnpacked mazeArray = // TODO: move to a module?

    let combinedWithDotsAndPillsIn (mazeArray:string[]) (justTheWalls:byte[]) =

        mazeArray |> IfValidStringRectangleThen (fun width height ->

            if (width * height) = justTheWalls.Length then
                let newArray = Array.create justTheWalls.Length ((byte)TileIndex.Blank)
                for y in 0..(height-1) do
                    for x in 0..(width-1) do
                        let i = y * width + x
                        let wall = justTheWalls.[i]
                        let tileIndex =
                            if wall <> 0uy then
                                wall + ((byte)TileIndex.Blank)
                            else
                                let ch = mazeArray.[y].[x]
                                if ch = '.' then
                                    ((byte) TileIndex.Dot)
                                else if ch = '@' then
                                    ((byte) TileIndex.Pill1)
                                else
                                    0uy
                        newArray.[i] <- tileIndex
                Some newArray
            else
                None
        )        

    let unwrap errorMessage opt =
        match opt with
            | Some thing -> thing
            | None       -> failwith errorMessage

    let justTheWalls =
        mazeArray 
            |> StringArrayToMazeByteArray (fun ch -> ch = '#' || ch = ':') 
            |> unwrap "Failed to unpack walls from maze definition, please check text format input."

    let theGhostRails =
        mazeArray 
            |> StringArrayToMazeByteArray (fun ch -> ch <> '#') 
            |> unwrap "Failed to unpack rails from maze definition, please check text format input."

    let thePlayersRails =
        mazeArray 
            |> StringArrayToMazeByteArray (fun ch -> ch <> '#' && ch <> ':') 
            |> unwrap "Failed to unpack rails from maze definition, please check text format input."

    let theWallsAndPills =
        justTheWalls 
            |> combinedWithDotsAndPillsIn mazeArray
            |> unwrap "Failed to obtains the dots and pills from maze definition, please check text format input."

    let unpackFindResult (ch:char) findResult =
        match findResult with
            | InvalidMaze -> failwith "Invalid maze definition"
            | NotFound    -> failwith (sprintf "Could not find char '%s' in the maze definition." (ch.ToString()))
            | NotUnique   -> failwith (sprintf "Char '%s' is not unique in the maze definition." (ch.ToString()))
            | Found(x,y)  -> 
                { ptx = x * TileSide ; pty = y * TileSide }

    let ghostPositions =
        [1..4] |> List.map (fun n -> 
            let ghostChar = (char) (48 + n)
            mazeArray |> FindUnique ghostChar |> unpackFindResult ghostChar)

    let pacPosition =
        mazeArray |> FindUnique '0' |> unpackFindResult '0'

    {
        UnpackedMazeState =
            {
                MazeTilesCountX  = mazeArray.[0].Length  // known valid
                MazeTilesCountY  = mazeArray.Length
                MazeTiles        = theWallsAndPills
                MazeGhostRails   = theGhostRails
                MazePlayersRails = thePlayersRails
            }
        UnpackedPacmanPosition = pacPosition
        UnpackedGhostPositions = ghostPositions
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private OriginForMazeOfDimensions cx cy (countX:int) (countY:int) =

    let x = cx - ((countX * TileSide) / 2)
    let y = cy - ((countY * TileSide) / 2)

    (x,y)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Support functions
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let KeysFrom keyStateGetter =
    let up    = (keyStateGetter (WebBrowserKeyCode 38)).Held
    let down  = (keyStateGetter (WebBrowserKeyCode 40)).Held
    let left  = (keyStateGetter (WebBrowserKeyCode 37)).Held
    let right = (keyStateGetter (WebBrowserKeyCode 39)).Held
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
let TileIndexOf position numTilesAcross =  // TODO: strongly type the return value

    if position |> IsAlignedOnTile then
        let {ptx=x ; pty=y} = position
        let txi = x / TileSide
        let tyi = y / TileSide
        Some (tyi * numTilesAcross + txi)

    else
        None

/// The railsByte defines permissable directions, and we return
/// true if movement in the given direction is allowed by the railsByte.
let IsDirectionAllowedBy railsByte facingDirection =

    (railsByte &&& (facingDirection |> FacingDirectionToMazeByte)) <> 0uy


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


/// Obtain the mode of pacman.
let inline PacMode pacman =
    pacman.PacState2.PacMode


/// Obtain pacman's bounding rectangle.
let inline PacBoundingRectangle pacman =
    pacman.PacPosition |> TileBoundingRectangle


/// Update pacman's mode.
let WithPacMode mode pacman =
    {
        PacPosition = pacman.PacPosition
        PacState2 =
            {
                PacMode            = mode
                PacFacingDirection = pacman.PacState2.PacFacingDirection
                LivesLeft          = pacman.PacState2.LivesLeft
            }
    }

    

/// Obtain the mode of the given ghost.
let inline GhostMode ghost =
    ghost.GhostState2.GhostMode


/// Obtain a ghost's bounding rectangle.
let inline GhostBoundingRectangle ghost =
    ghost.GhostPosition |> TileBoundingRectangle


/// Update the mode of the given ghost.
let WithGhostMode mode ghost =
    {
        GhostPosition = ghost.GhostPosition
        GhostState2   =
            {
                GhostNumber          = ghost.GhostState2.GhostNumber
                GhostHomePosition    = ghost.GhostState2.GhostHomePosition
                GhostFacingDirection = ghost.GhostState2.GhostFacingDirection
                GhostMode            = mode
            }
    }


/// Asks whether the given ghost is at its base home position.
let inline IsAtHomePosition ghost =
    ghost.GhostPosition = ghost.GhostState2.GhostHomePosition


/// Determine if two pacman-maze-tile sized areas overlap
/// given the top left positions of both.
let TilesOverlap tilePos1 tilePos2 =

    let {ptx=left1 ; pty=top1} = tilePos1
    let {ptx=left2 ; pty=top2} = tilePos2

    let intersects a b =
        let a' = a + TileSide
        let b' = b + TileSide
        not (b >= a' || b' <= a)

    left1 |> intersects left2  &&  top1 |> intersects top2


/// Asks whether a pacman-maze-tile sized area overlaps any
/// of the GhostNormal-state ghosts in the list.  Ghosts in
/// other states never participate in intersection.
/// The tilePos denotes the top left corner.
let TileIntersectsNormalGhostsIn ghostStateList tilePos =

    let intersects pos ghost =
        TilesOverlap pos ghost.GhostPosition

    ghostStateList |> List.exists (fun ghost ->
        match ghost.GhostState2.GhostMode with
            | GhostNormal -> ghost |> intersects tilePos
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


let OffsetByOrigin originx originy point =
    let { ptx=x ; pty=y } = point
    { ptx=x + originx ; pty=y + originy }


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  CORRIDOR DETERMINATION
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Returns the corridor rectangle starting from a given maze tile 'originTile',
/// where originTile is a 2D array index into the tiles matrix.  Takes the actual
/// maze, not the rails as the mazeByteArray.
let CorridorRectangle tilesHorizontally tilesVertically (mazeByteArray:byte[]) originTile direction =

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

let private DrawSpecificMazeCentred render image cx cy countX countY (mazeByteArray:byte[]) gameTime =

    let (x,y) = OriginForMazeOfDimensions cx cy countX countY

    for ty in 0..countY - 1 do
        let y' = y + ty * TileSide

        for tx in 0..countX - 1 do
            let tileIndex = mazeByteArray.[ty * countX + tx]
            let x' = x + tx * TileSide
            DrawPacTileInt render image x' y' ((int)tileIndex) gameTime

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private DrawMazeCentred render image cx cy mazeState gameTime =

    DrawSpecificMazeCentred 
        render image cx cy 
        mazeState.MazeTilesCountX
        mazeState.MazeTilesCountY
        mazeState.MazeTiles
        gameTime

    // DrawSpecificMazeCentred 
    //     render image cx cy 
    //     mazeState.MazeTilesCountX
    //     mazeState.MazeTilesCountY
    //     mazeState.MazeGhostRails
    //     gameTime

    // DrawSpecificMazeCentred 
    //     render image cx cy 
    //     mazeState.MazeTilesCountX
    //     mazeState.MazeTilesCountY
    //     mazeState.MazePlayersRails
    //     gameTime

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private DrawCorridorFinderResult render cx cy countX countY mazeByteArray pacPosHack =

    let (x,y) = OriginForMazeOfDimensions cx cy countX countY

    let facing = pacPosHack.PacState2.PacFacingDirection
    let pos    = pacPosHack.PacPosition
    let pos    = { ptx=pos.ptx / 16<epx> ; pty=pos.pty/16<epx> }
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
    //     model.PacmanState

    let (originx,originy) = 
        OriginForMazeOfDimensions 
            cx cy 
            model.MazeState.MazeTilesCountX 
            model.MazeState.MazeTilesCountY

    let pos = model.PacmanState.PacPosition 
                |> PointWrappedAtMazeEdges model.MazeState
                |> OffsetByOrigin originx originy 
    
    let direction = model.PacmanState.PacState2.PacFacingDirection

    match model.PacmanState.PacState2.PacMode with
        | PacAlive ->
            let pillMode = InPillMode model.GhostsState
            DrawPacManAlive render tilesImage pos direction pillMode gameTime

        | PacDyingUntil _ ->
            if gameTime |> PulseBetween PacmanDyingFlashRate true false then
                DrawPacManAlive render tilesImage pos direction false gameTime
            else
                () // No graphics desired.

        | PacDeadUntil _ ->
            () // No graphics desired.

    model.GhostsState
        |> List.iteri (fun i ghostState ->

            let pos = ghostState.GhostPosition
                        |> PointWrappedAtMazeEdges model.MazeState
                        |> OffsetByOrigin originx originy
                        
            let number = ghostState.GhostState2.GhostNumber
            let mode = ghostState.GhostState2.GhostMode

            DrawGhost render tilesImage pos number mode gameTime)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  SPYING - looking down the corridors
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
   



// let CanSpyPositionAt otherPosition maze fromPosition =

    


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
    let direction = pacmanState.PacState2.PacFacingDirection
    let mode      = pacmanState.PacState2.PacMode

    match mode with

        | PacAlive ->
            
            let struct (up, down, left, right) = KeysFrom keyStateGetter
            
            let directionImpliedByKeys  = KeyStatesToDirection up down left right direction
            let angleToCurrentDirection = AngleBetween directionImpliedByKeys direction

            let tile = TileIndexOf position mazeState.MazeTilesCountX

            let direction =
                match angleToCurrentDirection with
                    | ZeroAngle 
                    | AboutTurn180 -> directionImpliedByKeys

                    | AntiClockwiseTurn90
                    | ClockwiseTurn90 ->
                        match tile with
                            | None -> direction // disallow, not perfectly aligned
                            | Some tileIndex ->
                                if directionImpliedByKeys 
                                    |> IsDirectionAllowedBy mazeState.MazePlayersRails.[tileIndex] then
                                    directionImpliedByKeys
                                else
                                    direction // disallow, no exit in that direction.

            let eaten, scoreIncrement =
                match tile with
                    | None -> EatenNothing , 0u
                    | Some tileIndex ->
                        let tileType = mazeState.MazeTiles.[tileIndex]
                        if tileType = ((byte) TileIndex.Dot) then
                            (EatenDot tileIndex) , ScoreForEatingDot
                        else if tileType = ((byte) TileIndex.Pill1) then   // We don't store Pill2 in the matrix.
                            (EatenPowerPill tileIndex) , ScoreForEatingPowerPill
                        else
                            EatenNothing , 0u

            let position =  // TODO: issue of frame rate!

                let proposedPosition = 
                    position 
                        |> PointMovedByDelta (direction |> DirectionToMovementDeltaI32)
                        |> PointWrappedAtMazeEdges mazeState

                match tile with
                    | None -> proposedPosition // Can always allow movement when inbetween tiles
                    | Some tileIndex ->
                        if direction |> IsDirectionAllowedBy mazeState.MazePlayersRails.[tileIndex] then
                            proposedPosition
                        else
                            position // disallow, no exit in that direction.
            
            (eaten , position , direction , scoreIncrement)            


        | PacDyingUntil(_deadTime) ->
            (EatenNothing , position , direction , 0u)


        | PacDeadUntil(_endScreenTime) ->
            (EatenNothing , position , direction , 0u)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Ghost position advance
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private AdvanceGhost2 mazeState (allGhosts:GhostState list) (pacman:PacmanState) pacMask (ghost:GhostState) gameTime =

    // NB:  Only called for GhostNormal and GhostEdibleUntil cases.

    let position  = ghost.GhostPosition
    let direction = ghost.GhostState2.GhostFacingDirection
    let tile      = TileIndexOf position mazeState.MazeTilesCountX
    let rails     = mazeState.MazeGhostRails

    let direction =
        match tile with

            | None -> 
                direction

            | Some tile ->
                // Ghost precisely on a tile.  This is a decision point.

                let directionsGivenByRails = rails.[tile]

                // let potentialDirections =
                //   For each direction in directionsGivenByRails:  [essentially a fold.  Must be intelligent and NOT call handler if direction gets masked off by return]
                //     Get the corridor rectangle corridorRect
                //     If PAC intersects the corridorRect then:
                //       If we're GhostNormal then return the mask for this corridor's direction.
                //       Else if we're edible then return the input directions mask with this direction removed.  [Which may freeze this ghost, but that's OK]
                //       Else failwith "Should not have been called in this ghost mode"
                //     Else if any other ghost intersects the corridorRect then:
                //       return an updated mask with this corridor's direction eliminated.

                // If directions is now empty, then bail by restoring.

                // let direction =
                //   With the resulting directions:
                //   - If straight rail then keep moving in current direction.
                //   - Consider the number of directions given by the tile:
                //     + One = about turn always
                //     + Two = (corner) 80% probability of continue around corner, vs 20% about-turn
                //     + Three = 50% keep straight, 40% turn corner, 10% about-turn
                //     + Four = 50% keep straight, 20% turn corner 1, 20% turn corner 2, 10% about-turn
                //   ... But the weightings should be parameterised per ghost to give each one traits.

                direction

    let position = 
        position 
            |> PointMovedByDelta (direction |> DirectionToMovementDeltaI32)
            |> PointWrappedAtMazeEdges mazeState

    (position, direction)





let AdvanceToHomePosition ghost =

    let delta = 
        ghost.GhostPosition 
            |> SimpleMovementDeltaI32ToGetTo ghost.GhostState2.GhostHomePosition

    let position = 
        ghost.GhostPosition 
            |> PointMovedByDelta delta

    (position , ghost.GhostState2.GhostFacingDirection)


let private AdvanceGhost mazeState allGhosts pacman ghost gameTime =

    let (position , direction) =
        match ghost |> GhostMode with
            | GhostNormal ->
                AdvanceGhost2 mazeState allGhosts pacman 0x00 ghost gameTime

            | GhostEdibleUntil _ -> 
                AdvanceGhost2 mazeState allGhosts pacman 0x0F ghost gameTime

            | GhostReturningToBase ->
                AdvanceToHomePosition ghost

            | GhostRegeneratingUntil _ ->
                // No movement while re-generating in the base.
                (ghost.GhostPosition , ghost.GhostState2.GhostFacingDirection) 

    {
        GhostPosition = position
        GhostState2 =
            {
                GhostNumber          = ghost.GhostState2.GhostNumber
                GhostHomePosition    = ghost.GhostState2.GhostHomePosition
                GhostMode            = ghost.GhostState2.GhostMode
                GhostFacingDirection = direction
            }
    }


let private WithGhostMovement mazeState pacman gameTime allGhosts =

    allGhosts |> List.map (fun ghost -> AdvanceGhost mazeState allGhosts pacman ghost gameTime)


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Collisions PAC vs GHOSTS
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// State changes on pacman as a result of collision detection with ghosts
let WithStateChangesResultingFromCollisionWithGhosts ghostStateList gameTime pacmanState =

    match pacmanState.PacState2.PacMode with

        | PacAlive ->
            if pacmanState.PacPosition |> TileIntersectsNormalGhostsIn ghostStateList then
                pacmanState |> WithPacMode
                    (PacDyingUntil (gameTime + PacmanDyingAnimationTime))
            else
                pacmanState

        | PacDyingUntil t ->
            if gameTime >= t then 
                pacmanState |> WithPacMode (PacDeadUntil (gameTime + PacmanDeadPauseTime))
            else
                pacmanState

        | PacDeadUntil _ -> 
            // TODO: How do we handle the life loss?  Who handles it?
            pacmanState   // intersection does not apply



/// State changes on ghosts as a result of collision detection with pacman
let WithStateChangesResultingFromCollisionWithPacman pacmanPos ghosts =

    let isEdibleGhostOverlappingPacmanAt pacmanPos ghost =

        match ghost.GhostState2.GhostMode with
            | GhostEdibleUntil _        -> ghost.GhostPosition |> TilesOverlap pacmanPos
            | GhostNormal
            | GhostReturningToBase      
            | GhostRegeneratingUntil _  -> false

    // Garbage optimisation:  Scan for intersections and calculate score for eaten ghosts.

    let score = 
        ghosts |> List.sumBy (fun ghost ->
            if ghost |> isEdibleGhostOverlappingPacmanAt pacmanPos then ScoreForEatingGhost else NoScore)

    let ghosts = 
        if score = 0u then
            ghosts  // no ghosts changed state because none were eaten
        else
            ghosts |> List.map (fun ghost ->
                if ghost |> isEdibleGhostOverlappingPacmanAt pacmanPos then 
                    ghost |> WithGhostMode GhostReturningToBase
                else
                    ghost)

    struct (ghosts , score)


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  State update application
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// State update for pacman position and direction.
let WithPacManMovementStateChangesAppliedFrom position direction pacmanState =

    if direction = pacmanState.PacState2.PacFacingDirection then
        {
            PacState2 = pacmanState.PacState2  // unchanged
            PacPosition = position
        }
    else
        {
            PacState2 =
                {
                    PacMode            = pacmanState.PacState2.PacMode
                    PacFacingDirection = direction
                    LivesLeft          = pacmanState.PacState2.LivesLeft
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
                if gameTime >= t then ghost |> WithGhostMode GhostNormal else ghost

            | GhostReturningToBase ->
                if ghost |> IsAtHomePosition then
                    ghost |> WithGhostMode (GhostRegeneratingUntil (gameTime + RegenerationTime)) 
                else
                    ghost
    )





// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Screen state advance on frame
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextPacmanScreenState gameState keyStateGetter gameTime elapsed =

    // Unpack

    let model = ModelFrom gameState

    let {
            ScoreAndHiScore        = scoreAndHiScore
            MazeState              = mazeState
            PacmanState            = pacmanState
            GhostsState            = ghostStateList
            WhereToOnGameOver      = _
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
            |> WithGhostMovement mazeState pacmanState gameTime

    let pacmanState =
        pacmanState |> WithStateChangesResultingFromCollisionWithGhosts ghostStateList gameTime

    let struct (ghostStateList , scoreIncrement) =
        ghostStateList |> WithStateChangesResultingFromCollisionWithPacman pacmanState.PacPosition

    let scoreAndHiScore =
        scoreAndHiScore |> ScoreIncrementedBy scoreIncrement

    // Repack

    gameState |> WithUpdatedModel
        {
            ScoreAndHiScore   = scoreAndHiScore
            MazeState         = mazeState
            PacmanState       = pacmanState
            GhostsState       = ghostStateList
            WhereToOnGameOver = model.WhereToOnGameOver
        }        

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewPacmanScreen whereToOnGameOver scoreAndHiScore =

    let unpackedMaze = DefaultMaze |> TextMazeDefinitionUnpacked

    let pacModel =
        {
            ScoreAndHiScore   = scoreAndHiScore
            MazeState         = unpackedMaze.UnpackedMazeState
            WhereToOnGameOver = whereToOnGameOver

            // TODO: sort out
            PacmanState = { PacPosition = unpackedMaze.UnpackedPacmanPosition ; PacState2 = { PacFacingDirection = FacingRight ; PacMode = PacAlive ; LivesLeft = 3 } }
            GhostsState = unpackedMaze.UnpackedGhostPositions |> List.mapi (fun i ghostPos -> { GhostPosition = ghostPos ; GhostState2 = { GhostNumber = GhostNumber(i) ; GhostMode = GhostNormal ; GhostHomePosition = ghostPos ; GhostFacingDirection = FacingUp } })
        }

    NewGameState NextPacmanScreenState RenderPacmanScreen pacModel

