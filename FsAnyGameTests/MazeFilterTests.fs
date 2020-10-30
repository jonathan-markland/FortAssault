module MazeFilterTests

open Xunit
open FsXunit
open MazeFilter


// --------------------------------------------------------------------------------------------------------
//  Test support
// --------------------------------------------------------------------------------------------------------

let ToUnicodeBoxDrawing width (output:byte[]) =

    let unicodeBoxDrawing = " ╵╷│╴┘┐┤╶└┌├─┴┬┼"

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



let MazeToFSharpBoxDrawSourceCode isWall (maze:string[]) =

    let width = maze.[0].Length

    maze |> (StringArrayToMazeByteArray isWall)
         |> Option.map (fun maze -> maze |> ToFSharpBoxDrawSourceCodeSeq width |> String.concat "\r\n")

          



// --------------------------------------------------------------------------------------------------------
//  Test
// --------------------------------------------------------------------------------------------------------

let WhenTransformed isWall (maze:string[]) =

    let width = if maze.Length > 0 then maze.[0].Length else 0
    maze 
        |> (StringArrayToMazeByteArray isWall)
        |> Option.map (ToUnicodeBoxDrawing width)



// --------------------------------------------------------------------------------------------------------
//  MazeByteArray basic edge cases
// --------------------------------------------------------------------------------------------------------
        
[<Fact>]
let ``Empty input to MazeByteArray yields None`` () =
    [||] |> StringArrayToMazeByteArray IsHash |> ShouldEqual None

[<Fact>]
let ``Array of one empty string input to MazeByteArray yields None`` () =
    [| "" |] |> StringArrayToMazeByteArray IsHash |> ShouldEqual None

[<Fact>]
let ``Array of two empty string inputs to MazeByteArray yields None`` () =
    [| "" ; "" |] |> StringArrayToMazeByteArray IsHash |> ShouldEqual None



// --------------------------------------------------------------------------------------------------------
//  Standard maze filter
// --------------------------------------------------------------------------------------------------------

[<Fact>]
let ``MazeByteArray applied to basic maze produces correct pieces viewed via box drawing`` () =

    let maze =
        [|
            "########"
            "# #    #"
            "# # ## #"
            "# #  # #"
            "# #### #"
            "#      #"
            "########"
        |]

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
        [|
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
        |]

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
            "│ │         │ │"
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
let ``MazeByteArray applied to basic maze produces correct pieces with inverted logic when viewed via box drawing`` () =

    let maze =
        [|
            "########"
            "# #    #"
            "# # ## #"
            "# #  # #"
            "# #### #"
            "#      #"
            "########"
        |]

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
let ``MazeByteArray applied to comprehensive maze produces correct pieces with inverted logic when viewed via box drawing`` () =

    let maze =
        [|
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
        |]

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

    [||] |> WhenTransformed IsHash |> ShouldEqual None



[<Fact>]
let ``The 0 x 5 maze`` () =

    let maze = 
        [|
            ""
            ""
            ""
            ""
            ""
        |]

    maze |> WhenTransformed IsHash |> ShouldEqual None


