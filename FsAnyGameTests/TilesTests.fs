module TilesTests

open Xunit
open FsXunit
open Tiles

        // All tests are automatically run reflected in a 45-degree mirror,
        // so tests need only be written 'considered horizontally'.



// --------------------------------------------------------------------------------------------------------
//  Reflection
// --------------------------------------------------------------------------------------------------------

let ReflectedMatrix m = 
    {
        TilesHorizontally = m.TilesVertically
        TilesVertically   = m.TilesHorizontally
        TileWidthPixels   = m.TileHeightPixels
        TileHeightPixels  = m.TileWidthPixels
    }

let ReflectedWindow w =
    {
        WindowLeft   = w.WindowTop
        WindowTop    = w.WindowLeft
        WindowWidth  = w.WindowHeight
        WindowHeight = w.WindowWidth
    }

let ReflectedOffset o =
    {
        OffsetX = o.OffsetY
        OffsetY = o.OffsetX
    }


// --------------------------------------------------------------------------------------------------------
//  Test runner with reflection support
// --------------------------------------------------------------------------------------------------------

type OutputTile =
    {
        x  : int
        y  : int
        ix : int
        iy : int
    }

let OutputTileToString t =
    sprintf "(%d,%d) (ix%d,iy%d)" t.x t.y t.ix t.iy

type SortKey = { key1:int ; key2:int }

let SortedByIyThenIx outputTileList =
    outputTileList |> List.sortBy (fun {ix=ix;iy=iy} -> {key1=iy;key2=ix})

let ReflectedOutputTile t =
    {
        x  = t.y
        y  = t.x
        ix = t.iy
        iy = t.ix
    }

let InternalTilesResultReport matrix window offset =

    let mutable lst = []

    let report x y ix iy =
        let record = { x=x ; y=y ; ix=ix ; iy=iy }
        lst <- record::lst

    ForEachTileWithVisiblePortion matrix window offset report

    lst |> List.rev


let TilesResultReport matrix window offset =

    // Run "normal":
    let normal     = InternalTilesResultReport matrix window offset

    // Run reflected:
    let reflected  = InternalTilesResultReport 
                        (matrix |> ReflectedMatrix) 
                        (window |> ReflectedWindow) 
                        (offset |> ReflectedOffset)

    // Reflect the reflected result back again:
    let reflected' = reflected |> List.map ReflectedOutputTile

    // Certainly this should be the case, even if the caller decides the final result is wrong:
    reflected' |> SortedByIyThenIx |> ShouldEqual normal

    // Return normal result for futher checking:
    normal |> List.map OutputTileToString



// --------------------------------------------------------------------------------------------------------
//  Data
// --------------------------------------------------------------------------------------------------------

let Matrix1 = 
    {
        TilesHorizontally = 20
        TilesVertically   = 10
        TileWidthPixels   = 16
        TileHeightPixels  = 16
    }

let ZeroSizedWindow =
    {
        WindowLeft   = 50
        WindowTop    = 20
        WindowWidth  = 0
        WindowHeight = 0
    }

let WindowFourByFourTiles =
    {
        WindowLeft   = 50
        WindowTop    = 20
        WindowWidth  = 64
        WindowHeight = 64
    }

let WindowOnSingleTile =
    {
        WindowLeft   = 50
        WindowTop    = 20
        WindowWidth  = 16
        WindowHeight = 16
    }

let WindowOnThreeByTwoTiles =
    {
        WindowLeft   = 50
        WindowTop    = 20
        WindowWidth  = 16*3
        WindowHeight = 16*2
    }

let WindowOnTwoWideLessOnePixel =
    {
        WindowLeft   = 50
        WindowTop    = 20
        WindowWidth  = 16*2-1
        WindowHeight = 16
    }

let WindowOnTwoWidePlusOnePixel =
    {
        WindowLeft   = 50
        WindowTop    = 20
        WindowWidth  = 16*2+1
        WindowHeight = 16
    }


// --------------------------------------------------------------------------------------------------------
//  Zero-sized window
// --------------------------------------------------------------------------------------------------------


[<Fact>]
let ``Zero sized window never reports no tiles`` () =

    let tilePanningOffsetRelativeToWindow = { OffsetX = 0 ; OffsetY = 0 }

    TilesResultReport Matrix1 ZeroSizedWindow tilePanningOffsetRelativeToWindow
        |> ShouldEqual
            []


[<Fact>]
let ``Zero sized window with slight panning offset reports no tiles`` () =

    let tilePanningOffsetRelativeToWindow = { OffsetX = -10 ; OffsetY = -5 }

    TilesResultReport Matrix1 ZeroSizedWindow tilePanningOffsetRelativeToWindow
        |> ShouldEqual
            []


// --------------------------------------------------------------------------------------------------------
//  Window on a single tile
// --------------------------------------------------------------------------------------------------------


[<Fact>]
let ``Single tile window reports one tile when no offset`` () =

    let tilePanningOffsetRelativeToWindow = { OffsetX = 0 ; OffsetY = 0 }

    TilesResultReport Matrix1 WindowOnSingleTile tilePanningOffsetRelativeToWindow
        |> ShouldEqual
            ["(50,20) (ix0,iy0)"]



[<Fact>]
let ``Single tile window reports two tiles with horizontal slight offset`` () =

    let tilePanningOffsetRelativeToWindow = { OffsetX = -10 ; OffsetY = 0 }

    TilesResultReport Matrix1 WindowOnSingleTile tilePanningOffsetRelativeToWindow
        |> ShouldEqual
            ["(40,20) (ix0,iy0)" ; "(56,20) (ix1,iy0)"]



[<Fact>]
let ``Single tile window reports two tiles with horizontal slight offset (2)`` () =

    let tilePanningOffsetRelativeToWindow = { OffsetX = -(10+16) ; OffsetY = 0 }

    TilesResultReport Matrix1 WindowOnSingleTile tilePanningOffsetRelativeToWindow
        |> ShouldEqual
            ["(40,20) (ix1,iy0)"; "(56,20) (ix2,iy0)"]



[<Fact>]
let ``Single tile window reports two tiles with horizontal slight offset (3)`` () =

    let tilePanningOffsetRelativeToWindow = { OffsetX = -(10+32) ; OffsetY = 0 }

    TilesResultReport Matrix1 WindowOnSingleTile tilePanningOffsetRelativeToWindow
        |> ShouldEqual
            ["(40,20) (ix2,iy0)"; "(56,20) (ix3,iy0)"]



[<Fact>]
let ``Single tile window reports four tiles with dual offsets`` () =

    let tilePanningOffsetRelativeToWindow = { OffsetX = -10 ; OffsetY = -5 }

    TilesResultReport Matrix1 WindowOnSingleTile tilePanningOffsetRelativeToWindow
        |> ShouldEqual
            [
                "(40,15) (ix0,iy0)" ; "(56,15) (ix1,iy0)"
                "(40,31) (ix0,iy1)" ; "(56,31) (ix1,iy1)"
            ]



[<Fact>]
let ``Single tile window reports four tiles with dual offsets (2)`` () =

    let tilePanningOffsetRelativeToWindow = { OffsetX = -(10+64) ; OffsetY = -(5+128) }

    TilesResultReport Matrix1 WindowOnSingleTile tilePanningOffsetRelativeToWindow
        |> ShouldEqual
            [
                "(40,15) (ix4,iy8)" ; "(56,15) (ix5,iy8)"
                "(40,31) (ix4,iy9)" ; "(56,31) (ix5,iy9)"
            ]



// --------------------------------------------------------------------------------------------------------
//  4 x 4 tile window
// --------------------------------------------------------------------------------------------------------


[<Fact>]
let ``Tiles fitting perfectly in window with no tiles clipped gives 4 x 4 matrix`` () =

    let tilePanningOffsetRelativeToWindow = { OffsetX = 0 ; OffsetY = 0 }

    TilesResultReport Matrix1 WindowFourByFourTiles tilePanningOffsetRelativeToWindow
        |> ShouldEqual
            [
                 "(50,20) (ix0,iy0)" ; "(66,20) (ix1,iy0)" ; "(82,20) (ix2,iy0)"; "(98,20) (ix3,iy0)" ;
                 "(50,36) (ix0,iy1)" ; "(66,36) (ix1,iy1)" ; "(82,36) (ix2,iy1)"; "(98,36) (ix3,iy1)" ;
                 "(50,52) (ix0,iy2)" ; "(66,52) (ix1,iy2)" ; "(82,52) (ix2,iy2)"; "(98,52) (ix3,iy2)" ;
                 "(50,68) (ix0,iy3)" ; "(66,68) (ix1,iy3)" ; "(82,68) (ix2,iy3)"; "(98,68) (ix3,iy3)"
            ]



[<Fact>]
let ``Sliding tiles gives 5 x 5 matrix because of reveal of partial tiles`` () =

    let tilePanningOffsetRelativeToWindow = { OffsetX = -33 ; OffsetY = -17 }

    TilesResultReport Matrix1 WindowFourByFourTiles tilePanningOffsetRelativeToWindow
        |> ShouldEqual
            [
                "(49,19) (ix2,iy1)" ; "(65,19) (ix3,iy1)"; "(81,19) (ix4,iy1)"; "(97,19) (ix5,iy1)"; "(113,19) (ix6,iy1)";
                "(49,35) (ix2,iy2)" ; "(65,35) (ix3,iy2)"; "(81,35) (ix4,iy2)"; "(97,35) (ix5,iy2)"; "(113,35) (ix6,iy2)"; 
                "(49,51) (ix2,iy3)" ; "(65,51) (ix3,iy3)"; "(81,51) (ix4,iy3)"; "(97,51) (ix5,iy3)"; "(113,51) (ix6,iy3)";
                "(49,67) (ix2,iy4)" ; "(65,67) (ix3,iy4)"; "(81,67) (ix4,iy4)"; "(97,67) (ix5,iy4)"; "(113,67) (ix6,iy4)";
                "(49,83) (ix2,iy5)" ; "(65,83) (ix3,iy5)"; "(81,83) (ix4,iy5)"; "(97,83) (ix5,iy5)"; "(113,83) (ix6,iy5)"
            ]



// --------------------------------------------------------------------------------------------------------
//  3 x 2 tile window -- Index report tests
// --------------------------------------------------------------------------------------------------------

        // The Matrix1 defines 20*10 tile surface, giving rise to indices
        // ix0..ix19 and iy0..iy19.   We should never see requests for tiles
        // lying outside of this range no matter what the panning.


[<Fact>]
let ``We never report out of range indexes - no offsetting`` () =

    let tilePanningOffsetRelativeToWindow = { OffsetX = 0 ; OffsetY = 0 }

    TilesResultReport Matrix1 WindowOnThreeByTwoTiles tilePanningOffsetRelativeToWindow
        |> ShouldEqual
            [
                "(50,20) (ix0,iy0)" ; "(66,20) (ix1,iy0)" ; "(82,20) (ix2,iy0)" ;
                "(50,36) (ix0,iy1)" ; "(66,36) (ix1,iy1)" ; "(82,36) (ix2,iy1)"
            ]



[<Fact>]
let ``We never see request with ix minus 1`` () =

    let tilePanningOffsetRelativeToWindow = { OffsetX = 5 ; OffsetY = 0 }

    TilesResultReport Matrix1 WindowOnThreeByTwoTiles tilePanningOffsetRelativeToWindow
        |> ShouldEqual
            [
                "(55,20) (ix0,iy0)" ; "(71,20) (ix1,iy0)" ; "(87,20) (ix2,iy0)" ;
                "(55,36) (ix0,iy1)" ; "(71,36) (ix1,iy1)" ; "(87,36) (ix2,iy1)"
            ]



[<Fact>]
let ``We never see request with ix minus 1 or minus 2`` () =

    let tilePanningOffsetRelativeToWindow = { OffsetX = 32 ; OffsetY = 0 }

    TilesResultReport Matrix1 WindowOnThreeByTwoTiles tilePanningOffsetRelativeToWindow
        |> ShouldEqual
            [
                "(82,20) (ix0,iy0)" ;
                "(82,36) (ix0,iy1)"
            ]



[<Fact>]
let ``We never see request with ix beyond 19`` () =

    let tilePanningOffsetRelativeToWindow = { OffsetX = -(19*16) ; OffsetY = 0 }

    TilesResultReport Matrix1 WindowOnThreeByTwoTiles tilePanningOffsetRelativeToWindow
        |> ShouldEqual
            ["(50,20) (ix19,iy0)" ; "(50,36) (ix19,iy1)"]



// --------------------------------------------------------------------------------------------------------
//  3 x 2 tile window -- Sliding the data right off
// --------------------------------------------------------------------------------------------------------

        // Reminder - the auto reflection does the vertical directions.

[<Fact>]
let ``Slide data. Just off left`` () =
    TilesResultReport Matrix1 WindowOnThreeByTwoTiles { OffsetX = -(20*16) ; OffsetY = 0 }
        |> ShouldEqual []

[<Fact>]
let ``Slide data. Just off right`` () =
    TilesResultReport Matrix1 WindowOnThreeByTwoTiles { OffsetX = 20*16 ; OffsetY = 0 }
        |> ShouldEqual []

[<Fact>]
let ``Slide data. Completely off left`` () =
    TilesResultReport Matrix1 WindowOnThreeByTwoTiles { OffsetX = -((20*16)+1) ; OffsetY = 0 }
        |> ShouldEqual []

[<Fact>]
let ``Slide data. Completely off right`` () =
    TilesResultReport Matrix1 WindowOnThreeByTwoTiles { OffsetX = (20*16)+1 ; OffsetY = 0 }
        |> ShouldEqual []

[<Fact>]
let ``Slide data. Completely off left (2)`` () =
    TilesResultReport Matrix1 WindowOnThreeByTwoTiles { OffsetX = -(21*16) ; OffsetY = 0 }
        |> ShouldEqual []

[<Fact>]
let ``Slide data. Completely off right (2)`` () =
    TilesResultReport Matrix1 WindowOnThreeByTwoTiles { OffsetX = 21*16 ; OffsetY = 0 }
        |> ShouldEqual []


// --------------------------------------------------------------------------------------------------------
//  Irregular window sizes
// --------------------------------------------------------------------------------------------------------

[<Fact>]
let ``Irregular window one pixel less`` () =
    TilesResultReport Matrix1 WindowOnTwoWideLessOnePixel { OffsetX = 0 ; OffsetY = 0 }
        |> ShouldEqual ["(50,20) (ix0,iy0)"; "(66,20) (ix1,iy0)"]

[<Fact>]
let ``Irregular window one pixel less (2)`` () =
    TilesResultReport Matrix1 WindowOnTwoWideLessOnePixel { OffsetX = -1 ; OffsetY = 0 }
        |> ShouldEqual ["(49,20) (ix0,iy0)"; "(65,20) (ix1,iy0)"]

[<Fact>]
let ``Irregular window one pixel more`` () =
    TilesResultReport Matrix1 WindowOnTwoWidePlusOnePixel { OffsetX = 0 ; OffsetY = 0 }
        |> ShouldEqual ["(50,20) (ix0,iy0)"; "(66,20) (ix1,iy0)"; "(82,20) (ix2,iy0)"]

[<Fact>]
let ``Irregular window one pixel more (2)`` () =
    TilesResultReport Matrix1 WindowOnTwoWidePlusOnePixel { OffsetX = -1 ; OffsetY = 0 }
        |> ShouldEqual ["(49,20) (ix0,iy0)"; "(65,20) (ix1,iy0)"; "(81,20) (ix2,iy0)"]

[<Fact>]
let ``Irregular window one pixel more (3)`` () =
    TilesResultReport Matrix1 WindowOnTwoWidePlusOnePixel { OffsetX = 1 ; OffsetY = 0 }
        |> ShouldEqual ["(51,20) (ix0,iy0)"; "(67,20) (ix1,iy0)"]
