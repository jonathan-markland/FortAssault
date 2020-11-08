module MazeFilter

//   ########
//   # #    #
//   # # ####
//   # # #  #
//   ########



[<Struct>]
type private DirectionMasks =  // TODO: Unless MazeFilterIter is exposed publicly this is a much less important feature
    {
        UpMask          : byte
        DownMask        : byte
        LeftMask        : byte
        RightMask       : byte
        CentralDotIndex : byte
    }


let private NewDirectionMasks u d l r centralDotIndex =
    {
        UpMask          = u
        DownMask        = d
        LeftMask        = l
        RightMask       = r
        CentralDotIndex = centralDotIndex
    }


/// Validates that the string array is a non-empty rectangle of text
/// with the same number of characters on each line.  If so, returns
/// the dimensions.  Otherwise returns None.
let IfValidStringRectangleThen f (strings:string list) =

    match strings with
        | [] -> None
        | head::tail -> 
            let height = strings.Length
            let width = head.Length
            if width > 0 && strings |> List.forall (fun str -> str.Length = width) then
                f width height
            else
                None



type MazeByte = MazeByte of byte



/// Maze parser general algorithm.
/// Could be used to deduce four-way "box drawing" characters from 
/// a plain text matrix using a single character eg: # as a wall marker.
/// The input maze type is user-definable.  The output is a byte array
/// with user-definable bit values for the direction bits.
let private MazeFilterIter isWallAtXY width height masks action =  // TODO: action function is more complex than needed because the values come out in row-primary left-to-right order anyway.

    if width > 0 && height > 0 then

        let h = height - 1
        let w = width  - 1

        let setIfExitAt x y directionBitmask =
            
            /// A range is defined [0..e].
            /// The value n could step one place outside this range, 
            /// so if it does, wrap it back onto the other side:
            let inline withWraparound e n =
                if n < 0 then e else if n > e then 0 else n

            /// See if x or y are off the grid, if so wrap back 
            /// onto the opposite side:
            let (x,y) = (x |> withWraparound w , y |> withWraparound h)

            if isWallAtXY x y then directionBitmask else 0uy

        let horizontal = masks.LeftMask ||| masks.RightMask
        let vertical   = masks.UpMask ||| masks.DownMask

        let inline edgeMask where x a b mask =
            if where && (a ||| b) = mask then 0uy else x

        for y in 0..h do
            for x in 0..w do
                if isWallAtXY x y then
                    
                    let u = setIfExitAt x (y-1) masks.UpMask
                    let d = setIfExitAt x (y+1) masks.DownMask
                    let l = setIfExitAt (x-1) y masks.LeftMask
                    let r = setIfExitAt (x+1) y masks.RightMask

                    let u' = edgeMask (y=0) u l r horizontal
                    let l' = edgeMask (x=0) l u d vertical
                    let d' = edgeMask (y=h) d l r horizontal
                    let r' = edgeMask (x=w) r u d vertical

                    let bits = (u' ||| d' ||| l' ||| r')

                    let shapeIndex =
                        if bits = 0uy && (isWallAtXY x y) then
                            masks.CentralDotIndex
                        else
                            bits

                    action x y (MazeByte shapeIndex)
                else
                    action x y (MazeByte 0uy)  // To completely define the output


// TODO: Is there a bitmask type? [Flags] ?
let MazeByteUp    = 1uy
let MazeByteDown  = 2uy
let MazeByteLeft  = 4uy
let MazeByteRight = 8uy
let MazeByteCentralDotIndex = 16uy


/// Parse a 2D resource and return a row-primary byte array of 
/// maze path direction bitmasks, starting in the top left corner.
let private MazeArrayFromSource width height isWallAtXY (tileMapper:MazeByte -> 'userTile) (userTileDefault:'userTile) =

    if width > 0 && height > 0 then

        let masks  = NewDirectionMasks MazeByteUp MazeByteDown MazeByteLeft MazeByteRight MazeByteCentralDotIndex
        let output = Array.create<'userTile> (width * height) userTileDefault
        let action x y mask = output.[y * width + x] <- mask |> tileMapper

        MazeFilterIter isWallAtXY width height masks action

        Some output

    else None



/// Parse a textual representation of a maze from an array of equal-length 
/// strings, and return a row-primary byte array of direction bitmasks, 
/// starting in the top left corner.  All rows must be the same width for 
/// this to return a value, and the 'isWallChar' predicate determines which
/// squares in the input represent wall.
let StringArrayToMazeArray isWallChar (tileMapper:MazeByte -> 'userTile) (userTileDefault:'userTile) (maze:string list) =

    maze |> IfValidStringRectangleThen (fun width height ->
        MazeArrayFromSource width height (fun x y -> maze.[y].[x] |> isWallChar) tileMapper userTileDefault
    )

