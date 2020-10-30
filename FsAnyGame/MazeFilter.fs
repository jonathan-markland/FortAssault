module MazeFilter

//   ########
//   # #    #
//   # # ####
//   # # #  #
//   ########



[<Struct>]
type DirectionMasks =
    {
        UpMask    : byte
        DownMask  : byte
        LeftMask  : byte
        RightMask : byte
    }


let NewDirectionMasks u d l r =
    {
        UpMask    = u
        DownMask  = d
        LeftMask  = l
        RightMask = r
    }


/// Maze parser general algorithm.
/// Could be used to deduce four-way "box drawing" characters from 
/// a plain text matrix using a single character eg: # as a wall marker.
/// The input maze type is user-definable.  The output is a byte array
/// with user-definable bit values for the direction bits.
let MazeFilterIter isWallAtXY width height masks action =

    let test x y mask =
        if x >= 0 && y >= 0 && x < width && y < height then
            if isWallAtXY x y then mask else 0uy
        else
            0uy

        // let x = max 0 x
        // let x = min (width-1) x
        // let y = max 0 y
        // let y = min (height-1) y
        // if isWallAtXY x y then mask else 0uy



    for y in 0..(height-1) do
        for x in 0..(width-1) do
            if isWallAtXY x y then
                let u = test x (y-1) masks.UpMask
                let d = test x (y+1) masks.DownMask
                let l = test (x-1) y masks.LeftMask
                let r = test (x+1) y masks.RightMask
                action x y (u ||| d ||| l ||| r)
            else
                action x y 0uy  // To completely define the output



/// Parse a 2D resource and return a row-primary byte array of 
/// maze path direction bitmasks, starting in the top left corner.
let MazeByteArray width height isWallAtXY =

    if width > 0 && height > 0 then

        let masks  = NewDirectionMasks 1uy 2uy 4uy 8uy
        let output = Array.zeroCreate<byte> (width * height)
        let action x y mask = output.[y * width + x] <- mask

        MazeFilterIter isWallAtXY width height masks action

        Some output

    else None



/// Parse a textual representation of a maze from an array of equal-length 
/// strings, and return a row-primary byte array of direction bitmasks, 
/// starting in the top left corner.  All rows must be the same width for 
/// this to return a value, and the 'isWallChar' predicate determines which
/// squares in the input represent wall.
let StringArrayToMazeByteArray isWallChar (maze:string[]) =

    let height = maze.Length
    if height > 0 then

        let width = maze.[0].Length
        if width > 0 && maze |> Array.forall (fun str -> str.Length = width) then

            MazeByteArray width height (fun x y -> maze.[y].[x] |> isWallChar)

        else None

    else None

