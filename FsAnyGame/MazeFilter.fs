module MazeFilter

//   ########
//   # #    #
//   # # ####
//   # # #  #
//   ########



/// Bitmask scheme for 4-way directions, plus blank, and a single central point.
[<Struct>]
type MazeByte = MazeByte of byte

/// Bitmask value for an empty square.
let MazeByteEmpty = MazeByte 0uy

/// Bitmask value for left edge of 4-way direction.
/// Can be logically OR-ed with the other directions.
let MazeByteLeft  = MazeByte 8uy

/// Bitmask value for upward edge of 4-way direction.
/// Can be logically OR-ed with the other directions.
let MazeByteUp    = MazeByte 4uy

/// Bitmask value for right edge of 4-way direction.
/// Can be logically OR-ed with the other directions.
let MazeByteRight = MazeByte 2uy

/// Bitmask value for downward edge of 4-way direction.
/// Can be logically OR-ed with the other directions.
let MazeByteDown  = MazeByte 1uy

let MazeByteAll4Mask  = 15uy

/// Bitmask value for non-empty square with a single central column.
/// Must not be OR-ed with any other values.
let MazeByteCentralDotIndex = MazeByte 16uy

/// Logical OR for maze bitmasks.
let inline MazeByteMerge (MazeByte lhs) (MazeByte rhs) = 
    let result = (lhs ||| rhs)
    System.Diagnostics.Debug.Assert ((result &&& MazeByteAll4Mask) = result)   // Did you combine the MazeByteCentralDotIndex by any chance?
    MazeByte result

/// Logical AND for maze bitmasks in the 4-way range, or MazeByteEmpty.
let inline MazeAndMaskedWith (MazeByte lhs) (MazeByte rhs) = 
    System.Diagnostics.Debug.Assert ((lhs &&& MazeByteAll4Mask) = lhs)   // This should probably be in the 4-way flags range.
    System.Diagnostics.Debug.Assert ((rhs &&& MazeByteAll4Mask) = rhs)   // This should probably be in the 4-way flags range.
    MazeByte (lhs &&& rhs)
    
let inline IsNonEmpty4way (MazeByte b) =
    b >= 1uy && b <= 15uy

let inline IsValidRailBitmaskValue (MazeByte b) =
    b >= 0uy && b <= 16uy



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



/// Maze parser general algorithm.
/// Could be used to deduce four-way "box drawing" characters from 
/// a plain text matrix using a single character eg: # as a wall marker.
/// The input maze type is user-definable.  The output is a MazeByte array.
let private MazeFilterIter isWallAtXY width height action =

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

            if isWallAtXY x y then directionBitmask else MazeByteEmpty

        let horizontal = MazeByteMerge MazeByteLeft MazeByteRight
        let vertical   = MazeByteMerge MazeByteUp   MazeByteDown

        let inline edgeMask where x a b mask =
            if where && (MazeByteMerge a b) = mask then MazeByteEmpty else x

        for y in 0..h do
            for x in 0..w do
                if isWallAtXY x y then
                    
                    let l = setIfExitAt (x-1) y MazeByteLeft
                    let u = setIfExitAt x (y-1) MazeByteUp
                    let r = setIfExitAt (x+1) y MazeByteRight
                    let d = setIfExitAt x (y+1) MazeByteDown

                    let l' = edgeMask (x=0) l u d vertical
                    let u' = edgeMask (y=0) u l r horizontal
                    let r' = edgeMask (x=w) r u d vertical
                    let d' = edgeMask (y=h) d l r horizontal

                    let bits = (u' |> MazeByteMerge d' |> MazeByteMerge l' |> MazeByteMerge r')

                    let mazeByte =
                        if bits = MazeByteEmpty && (isWallAtXY x y) then
                            MazeByteCentralDotIndex
                        else
                            bits

                    action x y mazeByte
                else
                    action x y MazeByteEmpty  // In order to completely define the output


/// Parse a 2D resource and return a row-primary byte array of 
/// maze path direction bitmasks, starting in the top left corner.
let private MazeArrayFromSource width height isWallAtXY (tileMapper:MazeByte -> 'userTile) (userTileDefault:'userTile) =

    if width > 0 && height > 0 then

        let output = Array.create<'userTile> (width * height) userTileDefault
        let action x y mask = output.[y * width + x] <- mask |> tileMapper

        MazeFilterIter isWallAtXY width height action

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

