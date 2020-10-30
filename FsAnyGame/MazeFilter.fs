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
let MazeFilterIter isWall width height masks action =

    let test x y mask =
        if x >= 0 && y >= 0 && x < width && y < height then
            if isWall x y then mask else 0uy
        else
            0uy

    for y in 0..(height-1) do
        for x in 0..(width-1) do
            if isWall x y then
                let u = test x (y-1) masks.UpMask
                let d = test x (y+1) masks.DownMask
                let l = test (x-1) y masks.LeftMask
                let r = test (x+1) y masks.RightMask
                action x y (u ||| d ||| l ||| r)
            else
                action x y 0uy  // To completely define the output



/// Parse array of equal-length strings and return a row-primary
/// byte array of direction bitmasks, starting in the top left corner.
/// All rows must be the same width for this to return a value.
let MazeByteArray isWallChar (maze:string[]) =

    let height = maze.Length
    if height > 0 then

        let width = maze.[0].Length
        if width > 0 && maze |> Array.forall (fun str -> str.Length = width) then

            let isWall x y = isWallChar maze.[y].[x]
            let masks      = NewDirectionMasks 1uy 2uy 4uy 8uy
            let height     = maze.Length

            let output = Array.zeroCreate<byte> (width * height)
            let action x y mask = output.[y * width + x] <- mask

            MazeFilterIter isWall width height masks action

            Some output

        else None

    else None

