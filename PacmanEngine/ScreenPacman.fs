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
        "####################"
        "#@..##......##.....#"
        "#.#....####....#.#.#"
        "#.#.##########...#.#"
        "#.#............#.#.#"
        "#...###.####.#.#@..#"
        "#.#.#...#12:.#.#####"
        "#.#...#.#34:.#.....#"
        "#...###0####.#.#.#.#"
        "###.#..........#.#.#"
        "#...#.########.#.#.#"
        "#.#.#........#.....#"
        "#@#.#.#.####.#.#@#.#"
        "#.....#........#...#"
        "####################"
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
        UnpackedPacmanPosition : PointI32
        UnpackedGhostPositions : PointI32 list
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
                { ptix = x * TileSide ; ptiy = y * TileSide }

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

    let (originx,originy) = 
        OriginForMazeOfDimensions 
            cx cy 
            model.MazeState.MazeTilesCountX 
            model.MazeState.MazeTilesCountY

    let pillMode = InPillMode model.GhostsState

    DrawPacMan 
        render tilesImage originx originy model.PacmanState pillMode gameTime

    model.GhostsState
        |> List.iteri (fun i ghostState ->
            DrawGhost render tilesImage originx originy ghostState gameTime)

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

    let {ptix=x ; ptiy=y} = position
    let isAligned n = ((n |> IntEpxToInt) % TileSideInt) = 0

    x |> isAligned && y |> isAligned

/// For a precise pacman or ghost position that is precisely over
/// a single tile, this returns the rails array index of that tile.
/// If the position not perfectly aligned on a single tile, this
/// returns None.
let TileIndexOf position numTilesAcross =

    if position |> IsAlignedOnTile then
        let {ptix=x ; ptiy=y} = position
        let txi = x / TileSide
        let tyi = y / TileSide
        Some (tyi * numTilesAcross + txi)

    else
        None

/// The railsByte defines permissable directions, and we return
/// true if movement in the given direction is allowed by the railsByte.
let IsDirectionAllowedBy railsByte facingDirection =

    (railsByte &&& (facingDirection |> FacingDirectionToMazeByte)) <> 0uy
    

/// Obtain the mode of the given ghost.
let inline GhostMode ghost =
    ghost.GhostState2.GhostMode


/// Update the mode of the given ghost.
let inline WithGhostMode mode ghost =
    {
        GhostPosition = ghost.GhostPosition
        GhostState2   =
            {
                GhostNumber       = ghost.GhostState2.GhostNumber
                GhostHomePosition = ghost.GhostState2.GhostHomePosition
                GhostMode         = mode
            }
    }


/// Asks whether the given ghost is at its base home position.
let inline IsAtHomePosition ghost =
    ghost.GhostPosition = ghost.GhostState2.GhostHomePosition



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

                let potentialPosition = 
                    position |> PointI32MovedByDelta (direction |> DirectionToMovementDeltaI32)

                match tile with
                    | None -> potentialPosition // Can always allow movement when inbetween tiles
                    | Some tileIndex ->
                        if direction |> IsDirectionAllowedBy mazeState.MazePlayersRails.[tileIndex] then
                            potentialPosition
                        else
                            position // disallow, no exit in that direction.
            
            (eaten , position , direction , scoreIncrement)            


        | PacDyingUntil(_deadTime) ->
            (EatenNothing , position , direction , 0u)


        | PacDeadUntil(_endScreenTime) ->
            (EatenNothing , position , direction , 0u)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  State update application
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// State update for pacman position and direction.
let WithPacManStateChangesAppliedFrom position direction pacmanState =

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

    // Calculation

    let eaten , position , direction , scoreIncrement =
        AdvancePacMan keyStateGetter mazeState pacmanState

    let scoreAndHiScore =
        scoreAndHiScore |> ScoreIncrementedBy scoreIncrement

    // Application

    let pacmanState =
        pacmanState |> WithPacManStateChangesAppliedFrom position direction

    let mazeState =
        mazeState |> WithDotsRemovedFromArrayWhere eaten

    let ghostStateList =
        ghostStateList 
            |> WithReturnToNormalityIfTimeOutAt gameTime
            |> WithEdibleGhostsIfPowerPill eaten gameTime

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
            GhostsState = unpackedMaze.UnpackedGhostPositions |> List.mapi (fun i ghostPos -> { GhostPosition = ghostPos ; GhostState2 = { GhostNumber = GhostNumber(i) ; GhostMode = GhostNormal ; GhostHomePosition = ghostPos } })
        }

    NewGameState NextPacmanScreenState RenderPacmanScreen pacModel

