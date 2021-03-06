﻿module MazeFilterTests

open Xunit
open FsXunit
open MazeFilter



let RemoveMazeByteWrapper (MazeByte mazeByte) = mazeByte



// --------------------------------------------------------------------------------------------------------
//  Test support
// --------------------------------------------------------------------------------------------------------

let ToUnicodeBoxDrawing width (output:byte[]) =

    let unicodeBoxDrawing = " ╷╶┌╵│└├╴┐─┬┘┤┴┼●"

    let boxDrawOutput = 
        output 
            |> Array.map (fun n -> unicodeBoxDrawing.[int n]) 
            |> Array.chunkBySize width
            |> Array.toList
            |> List.map (fun arr -> new System.String(arr))

    boxDrawOutput



let ToFSharpBoxDrawSourceCodeSeq width output =

    let boxDraw = ToUnicodeBoxDrawing width output

    seq {
        yield "let expected ="
        yield "    ["
        yield! boxDraw |> List.map (fun str -> sprintf "        \"%s\"" str)
        yield "    ]"
    }



let IsHash = ((=) '#')
let IsNotHash = ((<>) '#')



let MazeToFSharpBoxDrawSourceCode isWall (mazeStrings:string list) =

    let width = mazeStrings.Head.Length

    mazeStrings |> (StringArrayToMazeArray isWall RemoveMazeByteWrapper 0uy)
         |> Option.map (
            fun maze -> maze |> ToFSharpBoxDrawSourceCodeSeq width |> String.concat "\r\n")

          



// --------------------------------------------------------------------------------------------------------
//  Test
// --------------------------------------------------------------------------------------------------------

let WhenTransformed isWall (maze:string list) =

    let width = if maze.Length > 0 then maze.Head.Length else 0
    maze 
        |> (StringArrayToMazeArray isWall RemoveMazeByteWrapper 0uy)
        |> Option.map (ToUnicodeBoxDrawing width)



// --------------------------------------------------------------------------------------------------------
//  MazeByteArray basic edge cases
// --------------------------------------------------------------------------------------------------------
        
[<Fact>]
let ``Empty input to MazeByteArray yields None`` () =
    [] |> StringArrayToMazeArray IsHash RemoveMazeByteWrapper 0uy |> ShouldEqual None

[<Fact>]
let ``Array of one empty string input to MazeByteArray yields None`` () =
    [ "" ] |> StringArrayToMazeArray IsHash RemoveMazeByteWrapper 0uy |> ShouldEqual None

[<Fact>]
let ``Array of two empty string inputs to MazeByteArray yields None`` () =
    [ "" ; "" ] |> StringArrayToMazeArray IsHash RemoveMazeByteWrapper 0uy |> ShouldEqual None



// --------------------------------------------------------------------------------------------------------
//  Standard maze filter
// --------------------------------------------------------------------------------------------------------

[<Fact>]
let ``MazeByteArray applied to basic maze produces correct pieces viewed via box drawing`` () =

    let maze =
        [
            "########"
            "# #    #"
            "# # ## #"
            "# #  # #"
            "# #### #"
            "#      #"
            "########"
        ]

    // Reminder of how to obtain F# source code:  
    let fssrc = maze |> MazeToFSharpBoxDrawSourceCode IsHash

    let expected =
        [
            "┌─┬────┐"
            "│ │    │"
            "│ │ ╶┐ │"
            "│ │  │ │"
            "│ └──┘ │"
            "│      │"
            "└──────┘"
        ]

    maze |> WhenTransformed IsHash |> ShouldEqual (Some expected)

// --------------------------------------------------------------------------------------------------------

[<Fact>]
let ``MazeByteArray applied to comprehensive case maze produces correct pieces viewed via box drawing`` () =

    let maze =
        [
            "###############"
            "#             #"
            "# ########### #"
            "# #   # #   # #"
            "# ##  #    ## #"
            "# #         # #"
            "# ###     ### #"
            "# #    #    # #"
            "# #         # #"
            "# #   #   ### #"
            "# # # #   ### #"
            "# ########### #"
            "#             #"
            "###############"
        ]

    // Reminder of how to obtain F# source code:  
    // let fssrc = maze |> MazeToFSharpBoxDrawSourceCode IsHash

    let expected =
        [
            "┌─────────────┐"
            "│             │"
            "│ ┌───┬─┬───┐ │"
            "│ │   │ ╵   │ │"
            "│ ├╴  ╵    ╶┤ │"
            "│ │         │ │"
            "│ ├─╴     ╶─┤ │"
            "│ │    ●    │ │"
            "│ │         │ │"
            "│ │   ╷   ┌┬┤ │"
            "│ │ ╷ │   ├┼┤ │"
            "│ └─┴─┴───┴┴┘ │"
            "│             │"
            "└─────────────┘"
        ]

    maze |> WhenTransformed IsHash |> ShouldEqual (Some expected)

// --------------------------------------------------------------------------------------------------------

[<Fact>]
let ``MazeByteArray applied to maze with holes at edges`` () =

    let maze =
        [
            "#### # #"
            "  #     "
            "# # ## #"
            "  #  # #"
            "# #### #"
            "#       "
            "# ## ###"
        ]

    // Reminder of how to obtain F# source code:  
    let fssrc = maze |> MazeToFSharpBoxDrawSourceCode IsHash

    let expected =
        [
            "──┬┘ ╵ └"
            "  │     "
            "╴ │ ╶┐ ┌"
            "  │  │ │"
            "┐ └──┘ └"
            "│       "
            "│ ┌┐ ┌──"
        ]

    maze |> WhenTransformed IsHash |> ShouldEqual (Some expected)

// --------------------------------------------------------------------------------------------------------
//  Inverted logic tests
// --------------------------------------------------------------------------------------------------------

[<Fact>]
let ``MazeByteArray applied to basic maze produces correct pieces with inverted logic when viewed via box drawing`` () =

    let maze =
        [
            "########"
            "# #    #"
            "# # ## #"
            "# #  # #"
            "# #### #"
            "#      #"
            "########"
        ]

    // Reminder of how to obtain F# source code:  
    // let fssrc = maze |> MazeToFSharpBoxDrawSourceCode IsNotHash

    let expected =
        [
            "        "
            " ╷ ┌──┐ "
            " │ │  │ "
            " │ └╴ │ "
            " │    │ "
            " └────┘ "
            "        "
        ]

    maze |> WhenTransformed IsNotHash |> ShouldEqual (Some expected)

// --------------------------------------------------------------------------------------------------------

[<Fact>]
let ``MazeByteArray applied to maze with holes at edges inverted`` () =

    // Some holes allow wrap-around because they align, some don't.

    let maze =
        [
            "#### # #"
            "  #     "
            "# # ## #"
            "  #  # #"
            "# #### #"
            "#       "
            "# ## ###"
        ]

    // Reminder of how to obtain F# source code:  
    let fssrc = maze |> MazeToFSharpBoxDrawSourceCode IsNotHash

    let expected =
        [
            "    │ ╷ "
            "─┐ ┌┴─┼─"
            " │ │  │ "
            "╶┤ └╴ │ "
            " │    │ "
            " ├──┬─┴╴"
            " ╵  │   "
        ]

    maze |> WhenTransformed IsNotHash |> ShouldEqual (Some expected)

// --------------------------------------------------------------------------------------------------------

[<Fact>]
let ``MazeByteArray applied to comprehensive maze produces correct pieces with inverted logic when viewed via box drawing`` () =

    let maze =
        [
            "###############"
            "#             #"
            "# ########### #"
            "# #   # #   # #"
            "# ##  #    ## #"
            "# #         # #"
            "# ###     ### #"
            "# #    #    # #"
            "# #         # #"
            "# #   #   ### #"
            "# # # #   ### #"
            "# ########### #"
            "#             #"
            "###############"
        ]

    // Reminder of how to obtain F# source code:  
    // let fssrc = maze |> MazeToFSharpBoxDrawSourceCode IsNotHash

    let expected =
        [
            "               "
            " ┌───────────┐ "
            " │           │ "
            " │ ╶┬┐ ╷ ┌┬╴ │ "
            " │  ├┤ ├┬┼┤  │ "
            " │ ╶┴┼┬┼┼┼┴╴ │ "
            " │   ├┼┴┼┤   │ "
            " │ ┌┬┼┤ ├┼┬┐ │ "
            " │ ├┼┼┴┬┼┼┴┘ │ "
            " │ ├┴┤ ├┼┤   │ "
            " │ ╵ ╵ └┴┘   │ "
            " │           │ "
            " └───────────┘ "
            "               "
        ]

    maze |> WhenTransformed IsNotHash |> ShouldEqual (Some expected)





// --------------------------------------------------------------------------------------------------------
//  Edge cases
// --------------------------------------------------------------------------------------------------------

[<Fact>]
let ``The 0 x 0 maze`` () =

    [] |> WhenTransformed IsHash |> ShouldEqual None



[<Fact>]
let ``The 0 x 5 maze`` () =

    let maze = 
        [
            ""
            ""
            ""
            ""
            ""
        ]

    maze |> WhenTransformed IsHash |> ShouldEqual None



[<Fact>]
let ``Getting walls from the 5 x 5 completely empty maze`` () =

    let maze = 
        [
            "     "
            "     "
            "     "
            "     "
            "     "
        ]

    maze |> WhenTransformed IsHash |> ShouldEqual (Some maze)


[<Fact>]
let ``Getting rails from the 5 x 5 completely empty maze`` () =

    let maze = 
        [
            "     "
            "     "
            "     "
            "     "
            "     "
        ]

    // Reminder of how to obtain F# source code:  
    // let fssrc = maze |> MazeToFSharpBoxDrawSourceCode IsNotHash

    let expected =
        [
            "┌┬┬┬┐"
            "├┼┼┼┤"
            "├┼┼┼┤"
            "├┼┼┼┤"
            "└┴┴┴┘"
        ]

    maze |> WhenTransformed IsNotHash |> ShouldEqual (Some expected)


[<Fact>]
let ``Getting rails from the 1 x 5 completely empty maze`` () =

    let maze = 
        [
            " "
            " "
            " "
            " "
            " "
        ]

    // Reminder of how to obtain F# source code:  
    // let fssrc = maze |> MazeToFSharpBoxDrawSourceCode IsNotHash

    let expected =
        [
            "╷"
            "│"
            "│"
            "│"
            "╵"
        ]

    maze |> WhenTransformed IsNotHash |> ShouldEqual (Some expected)


[<Fact>]
let ``Getting walls from the 1 x 1 completely empty maze`` () =

    [" "] |> WhenTransformed IsHash |> ShouldEqual (Some [" "])


[<Fact>]
let ``Getting rails from the 1 x 1 completely empty maze`` () =

    [" "] |> WhenTransformed IsNotHash |> ShouldEqual (Some ["●"])



