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



let MazeByteArrayUsingHashAsWall = 
    MazeByteArray (fun ch -> ch = '#')



let MazeToFSharpBoxDrawSourceCode (maze:string[]) =

    let width = maze.[0].Length

    maze |> MazeByteArrayUsingHashAsWall
         |> Option.map (fun maze -> maze |> ToFSharpBoxDrawSourceCodeSeq width |> String.concat "\r\n")

          



// --------------------------------------------------------------------------------------------------------
//  Test
// --------------------------------------------------------------------------------------------------------

let WhenTransformed (maze:string[]) =

    let width = if maze.Length > 0 then maze.[0].Length else 0
    maze 
        |> MazeByteArrayUsingHashAsWall
        |> Option.map (ToUnicodeBoxDrawing width)



// --------------------------------------------------------------------------------------------------------
//  MazeByteArray basic edge cases
// --------------------------------------------------------------------------------------------------------
        
[<Fact>]
let ``Empty input to MazeByteArray yields None`` () =
    [||] |> MazeByteArrayUsingHashAsWall |> ShouldEqual None

[<Fact>]
let ``Array of one empty string input to MazeByteArray yields None`` () =
    [| "" |] |> MazeByteArrayUsingHashAsWall |> ShouldEqual None

[<Fact>]
let ``Array of two empty string inputs to MazeByteArray yields None`` () =
    [| "" ; "" |] |> MazeByteArrayUsingHashAsWall |> ShouldEqual None



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

    maze |> WhenTransformed |> ShouldEqual (Some expected)

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
    // let fssrc = maze |> MazeToFSharpBoxDrawSourceCode

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

    maze |> WhenTransformed |> ShouldEqual (Some expected)

// --------------------------------------------------------------------------------------------------------
//  Edge cases
// --------------------------------------------------------------------------------------------------------

[<Fact>]
let ``The 0 x 0 maze`` () =

    [||] |> WhenTransformed |> ShouldEqual None



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

    maze |> WhenTransformed |> ShouldEqual None


