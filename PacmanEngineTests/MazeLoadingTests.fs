module Tests

open Xunit
open FsXunit
open MazeUnpacker
open MazeState
open MazeFilter



let ShouldLoadOk maze =

    let {
            UnpackedMazeState      = unpackedMazeState
            UnpackedPacmanPosition = pacmanPosition
            UnpackedGhostPositions = ghostPositions

        } = maze |> TextMazeDefinitionUnpacked

    let {
            MazeTilesCountX  = countX
            MazeTilesCountY  = countY
            MazeTiles        = tiles
            MazeGhostRails   = ghostRails 
            MazePlayersRails = playerRails

        } = unpackedMazeState

    countX |> ShouldEqualInt 20
    countY |> ShouldEqualInt 15

    ghostPositions.Length |> ShouldEqualInt 4

    tiles.Length |> ShouldEqual (20 * 15)
    ghostRails.Length |> ShouldEqual (20 * 15)
    playerRails.Length |> ShouldEqual (20 * 15)

    let checkRailsFor where =
        Array.iteri 
            (fun i bitmask -> 
                bitmask 
                    |> IsValidRailBitmaskValue 
                    |> ShouldBeTrueWhenDoing (sprintf "Examining %s rails bitmask at index [%d]" where i))

    ghostRails |> checkRailsFor "ghost"
    playerRails |> checkRailsFor "player"



[<Fact>]
let ``Maze load succeeds for known good`` () =

    [
        "###.# #.# ##########"
        "###.#...#.##########"
        "###.#.#.#....@....##"
        "....#.#.#.#######..."
        "#####.#...........##"
        "#...#.#.####.#######"
        "#.#.#.#.#12:.#....@#"
        "#@#...#.#34:.#.###.#"
        "#.#0#.#.####.#.###.#"
        "#...#.#............#"
        "#####.#.############"
        "....#.#............."
        "###.#.#.##########.#"
        "###.#...#.........@#"
        "###.# #.# ##########"
    ]
        |> WhenInputTo ShouldLoadOk
        |> ShouldPass



[<Fact>]
let ``All the mazes in 'Mazes' module load OK`` () =

    Mazes.AllPacmanMazes 
        |> Array.iteri 
            (fun i mazeStringList -> 
                mazeStringList 
                    |> WhenInputTo ShouldLoadOk
                    |> InContext (sprintf "loading Mazes.AllPacmanMazes.[%d]" i)
                    |> ShouldPass)



[<Fact>]
let ``Maze load fails for non rectangular`` () =

    [
        "###.# #.# ##########"
        "###.#...#.##########"
        "###.#.#.#....@....##"
        "....#.#.#.#######..."
        "#####.#...........##"
        "#...#.#.####.#######"
        "#.#.#.#.#12:.#....@##"
        "#@#...#.#34:.#.###.#"
        "#.#0#.#.####.#.###.#"
        "#...#.#............#"
        "#####.#.############"
        "....#.#............."
        "###.#.#.##########.#"
        "###.#...#.........@#"
        "###.# #.# ##########"
    ]
        |> WhenInputTo ShouldLoadOk
        |> ShouldFailWithAnException



[<Fact>]
let ``Maze load fails without pacman being specified`` () =

    [
        "###.# #.# ##########"
        "###.#...#.##########"
        "###.#.#.#....@....##"
        "....#.#.#.#######..."
        "#####.#...........##"
        "#...#.#.####.#######"
        "#.#.#.#.#12:.#....@#"
        "#@#...#.#34:.#.###.#"
        "#.#.#.#.####.#.###.#"  // No '0' anywhere!
        "#...#.#............#"
        "#####.#.############"
        "....#.#............."
        "###.#.#.##########.#"
        "###.#...#.........@#"
        "###.# #.# ##########"
    ]
        |> WhenInputTo ShouldLoadOk
        |> ShouldFailWithExceptionMessageWhere ((=) "Could not find char '0' in the maze definition.")



[<Fact>]
let ``Maze load fails without ghost 1 being specified`` () =

    [
        "###.# #.# ##########"
        "###.#...#.##########"
        "###.#.#.#....@....##"
        "....#.#.#.#######..."
        "#####.#...........##"
        "#...#.#.####.#######"
        "#.#.#.#.# 2:.#....@#"
        "#@#...#.#34:.#.###.#"
        "#.#0#.#.####.#.###.#"
        "#...#.#............#"
        "#####.#.############"
        "....#.#............."
        "###.#.#.##########.#"
        "###.#...#.........@#"
        "###.# #.# ##########"
    ]
        |> WhenInputTo ShouldLoadOk
        |> ShouldFailWithExceptionMessageWhere ((=) "Could not find char '1' in the maze definition.")
