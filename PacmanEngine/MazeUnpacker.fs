module MazeUnpacker

open Geometry
open MazeFilter
open ResourceIDs
open MazeState



type UnpackedMaze =
    {
        UnpackedMazeState      : MazeState
        UnpackedPacmanPosition : Point<int<epx>>
        UnpackedGhostPositions : Point<int<epx>> list
    }



type private FindResult = InvalidMaze | NotFound | Found of int * int | NotUnique

let private FindUnique charToFind mazeArray =

    let result =
        mazeArray |> IfValidStringRectangleThen (fun width height ->

            let mutable findResult = NotFound

            mazeArray |> List.iteri (fun y row ->
                for x in 0..(width-1) do
                    if row.[x] = charToFind then
                        findResult <-
                            match findResult with
                                | NotFound    -> Found(x,y)
                                | Found _     -> NotUnique
                                | NotUnique   -> NotUnique
                                | InvalidMaze -> failwith "unexpected case"  // should never happen
            )
            Some findResult
        )

    match result with
        | Some findResult -> findResult
        | None -> InvalidMaze



let TextMazeDefinitionUnpacked mazeList =

    // TODO: The rails might return the "central dot"

    // TODO: Have I been too hasty assuming the characters will never encounter
    //       rails with a direction byte mask 0uy?

    let combinedWithDotsAndPillsIn (mazeList:string list) (justTheWalls:MazeTile[]) =

        mazeList |> IfValidStringRectangleThen (fun width height ->

            if (width * height) = justTheWalls.Length then
                let newArray = Array.create justTheWalls.Length (MazeTile (byte TileIndex.Blank))
                mazeList |> List.iteri (fun y row ->
                    for x in 0..(width-1) do
                        let i = y * width + x
                        let (MazeTile wall) = justTheWalls.[i]
                        let tileIndex =
                            if wall <> 0uy then
                                wall + ((byte)TileIndex.Blank)
                            else
                                let ch = row.[x]
                                if ch = '.' then
                                    ((byte) TileIndex.Dot)
                                else if ch = '@' then
                                    ((byte) TileIndex.Pill1)
                                else
                                    0uy
                        newArray.[i] <- MazeTile tileIndex
                )
                Some newArray
            else
                None
        )        

    let unwrap errorMessage opt =
        match opt with
            | Some thing -> thing
            | None       -> failwith errorMessage

    let mazeByteToMazeTile (MazeByte(b)) = MazeTile b   // TODO: This is a Pacman system rule that should be stated somewhere more globally.

    let justTheWalls =
        mazeList 
            |> StringArrayToMazeArray (fun ch -> ch = '#' || ch = ':') mazeByteToMazeTile (MazeTile 0uy)
            |> unwrap "Failed to unpack walls from maze definition, please check text format input."

    let theGhostRails =
        mazeList 
            |> StringArrayToMazeArray (fun ch -> ch <> '#')  id MazeByteEmpty
            |> unwrap "Failed to unpack rails from maze definition, please check text format input."

    let thePlayersRails =
        mazeList 
            |> StringArrayToMazeArray (fun ch -> ch <> '#' && ch <> ':')  id MazeByteEmpty
            |> unwrap "Failed to unpack rails from maze definition, please check text format input."

    let theWallsAndPills =
        justTheWalls 
            |> combinedWithDotsAndPillsIn mazeList
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
            mazeList |> FindUnique ghostChar |> unpackFindResult ghostChar)

    let pacPosition =
        mazeList |> FindUnique '0' |> unpackFindResult '0'

    {
        UnpackedMazeState =
            {
                MazeTilesCountX  = mazeList.[0].Length  // known valid
                MazeTilesCountY  = mazeList.Length
                MazeTiles        = theWallsAndPills
                MazeGhostRails   = theGhostRails
                MazePlayersRails = thePlayersRails
            }
        UnpackedPacmanPosition = pacPosition
        UnpackedGhostPositions = ghostPositions
    }
