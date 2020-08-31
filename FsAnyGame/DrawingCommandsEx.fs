module DrawingCommandsEx

open DrawingCommands
open FontCharacterPositioning
open Geometry
open Time

// ---------------------------------------------------------------------------------------------------------
//  Drawing bitmap images
// ---------------------------------------------------------------------------------------------------------

/// Image collection specification, for use with the CycleImages drawing function.
type ImageWithDimensions =
    {
        ImageID     : ImageID
        ImageWidth  : float32<wu>  // TODO: make int?
        ImageHeight : float32<wu>
    }

/// Draw image without stretching.
let Image1to1 render left top imageResource =
    render (DrawImageWithTopLeftAtInt(left, top, imageResource))

/// Draw image, stretched to fit target rectangle.
let ImageStretched render left top imageResource destWidth destHeight =
    render (DrawStretchedImageWithTopLeftAt(left, top, imageResource, destWidth, destHeight))

/// Draw image centered about a point, without stretching.
let CentreImage render cx cy imageWithDimensions =
    let {ImageID=imageResource ; ImageWidth=w ; ImageHeight=h} = imageWithDimensions
    let left  = cx - (w / 2.0F)
    let top   = cy - (h / 2.0F)
    render (DrawStretchedImageWithTopLeftAt(left, top, imageResource, w, h))

/// Draw image centered about a point, without stretching.
/// The image is one of a repeating animation collection.
let CycleImages render cx cy (imageArray:ImageWithDimensions array) (fullCycleDuration:float32<seconds>) (elapsed:float32<seconds>) =
    let numImages    = imageArray.Length
    let timePerImage = fullCycleDuration / (float32 numImages)
    let index        = (abs (int (elapsed / timePerImage))) % numImages
    if index < numImages then
        CentreImage render cx cy (imageArray.[index])

// ---------------------------------------------------------------------------------------------------------
//  Geometric shape drawing
// ---------------------------------------------------------------------------------------------------------

/// Draw a solid filled rectangle.
let Rectangle render left top width height colour =
    render (DrawFilledRectangle(left, top, width, height, colour))


/// Draw a square of a given side, centred about a point.
let SquareAroundPoint render left top (side:int<wu>) colour =
    let h = side / 2
    Rectangle render (left - h) (top - h) side side colour

// ---------------------------------------------------------------------------------------------------------
//  Drawing text
// ---------------------------------------------------------------------------------------------------------

/// Draw text string in a given font, aligned in a given way with respect to a point.
let Text render fontResource hAlign vAlign (x:int<wu>) (y:int<wu>) message =
   
    let drawCharImage (index:int) left top =
        // TODO: Should algorithm use uint32 for the char index?
        render (DrawCharImageWithTopLeftAt(IntToIntWu left, IntToIntWu top, uint32 index, fontResource))
    
    // TODO: Constants?
    LayOutMonospaceFontTextString drawCharImage 8 8 (IntWuToInt x) (IntWuToInt y) message hAlign vAlign  



/// Draw a uint32 in a given font, aligned in a given way with respect to a point.
let Num render fontResource hAlign vAlign x y (value:uint32) =
    Text render fontResource hAlign vAlign x y (value.ToString())



/// Draw a float32 in a given font, aligned in a given way with respect to a point.
let Flo render fontResource hAlign vAlign x y (value:float32) =
    // TODO: the format string should be parameterized:
    Text render fontResource hAlign vAlign x y (value.ToString("#.##"))  


// ---------------------------------------------------------------------------------------------------------
//  Drawing paragraph
// ---------------------------------------------------------------------------------------------------------

let Paragraph render fontResource hAlign vAlign (x:int<wu>) (y:int<wu>) (ydelta:int<wu>) messageList =

    messageList
        |> List.iteri (fun i message ->
            let y = y + ydelta * i
            Text render fontResource hAlign vAlign x y message)

