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
        MazeTiles        : byte[]  // content mutated by pacman eating things
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
                let h = TileSide / 2
                { ptix = x * TileSide + h ; ptiy = y * TileSide + h }

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

let private DrawSpecificMazeCentred render image cx cy countX countY (mazeByteArray:byte[]) =

    let (x,y) = OriginForMazeOfDimensions cx cy countX countY

    for ty in 0..countY - 1 do
        let y' = y + ty * TileSide

        for tx in 0..countX - 1 do
            let tileIndex = mazeByteArray.[ty * countX + tx]
            let x' = x + tx * TileSide
            DrawPacTileInt render image x' y' ((int)tileIndex)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private DrawMazeCentred render image cx cy mazeState =

    DrawSpecificMazeCentred 
        render image cx cy 
        mazeState.MazeTilesCountX
        mazeState.MazeTilesCountY
        mazeState.MazeTiles

    // DrawSpecificMazeCentred 
    //     render image cx cy 
    //     mazeState.MazeTilesCountX
    //     mazeState.MazeTilesCountY
    //     mazeState.MazeGhostRails

    // DrawSpecificMazeCentred 
    //     render image cx cy 
    //     mazeState.MazeTilesCountX
    //     mazeState.MazeTilesCountY
    //     mazeState.MazePlayersRails

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

    let (originx,originy) = 
        OriginForMazeOfDimensions 
            cx cy 
            model.MazeState.MazeTilesCountX 
            model.MazeState.MazeTilesCountY

    let pillMode = AnyGhostsAreEdibleIn model.GhostsState

    DrawPacMan 
        render tilesImage originx originy model.PacmanState pillMode gameTime

    model.GhostsState
        |> List.iteri (fun i ghostState ->
            DrawGhost render tilesImage originx originy ghostState gameTime)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextPacmanScreenState gameState keyStateGetter gameTime elapsed =

    //   let up    = (keyStateGetter (WebBrowserKeyCode 38)).Held
    //   let down  = (keyStateGetter (WebBrowserKeyCode 40)).Held
    //   let left  = (keyStateGetter (WebBrowserKeyCode 37)).Held
    //   let right = (keyStateGetter (WebBrowserKeyCode 39)).Held
    //
    //   let model = ModelFrom gameState


    Unchanged gameState

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

