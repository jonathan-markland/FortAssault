module DrawingCommandsEx

open DrawingCommands
open FontCharacterPositioning
open Geometry
open Time
open ResourceFileMetadata

open StaticResourceAccess // TODO: needed for font solution because we pass IDs not objects -- may revisit!

// ---------------------------------------------------------------------------------------------------------
//  Drawing bitmap images
// ---------------------------------------------------------------------------------------------------------

/// Draw image without stretching.
let Image1to1 render left top imageWithHostObject =
    render (DrawImageWithTopLeftAtInt(left, top, imageWithHostObject))

/// Draw image, stretched to fit target rectangle.
let ImageStretched render left top imageWithHostObject destWidth destHeight =
    render (DrawStretchedImageWithTopLeftAt(left, top, imageWithHostObject, destWidth, destHeight))

/// Draw image centered about a point, without stretching.
let CentreImage render cx cy (imageWithHostObject:ImageWithHostObject) =
    let (w,h) = imageWithHostObject |> ImageDimensionsF
    let left  = cx - (w / 2.0F)
    let top   = cy - (h / 2.0F)
    render (DrawStretchedImageWithTopLeftAt(left, top, imageWithHostObject, w, h))

/// Draw image centered about a point, without stretching.
/// The image is one of a repeating animation collection.
let CycleImages render cx cy (imageArray:ImageWithHostObject array) (fullCycleDuration:float32<seconds>) (elapsed:float32<seconds>) =
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
let SquareAroundPoint render left top (side:int<epx>) colour =
    let h = side / 2
    Rectangle render (left - h) (top - h) side side colour

// ---------------------------------------------------------------------------------------------------------
//  Drawing text
// ---------------------------------------------------------------------------------------------------------

let private DrawCharImageWithTopLeftAt render (x:int) (y:int) charIndex (fontDefinition:FontWithHostObject) =

    let cwid = fontDefinition.CharWidth
    let chei = fontDefinition.CharHeight
    let chx  = (int charIndex) * cwid // TODO: constant: assuming char with for fonts.

    render (DrawSubImageStretchedToTarget(
                chx, 0, cwid, chei,
                (x |> IntToFloatEpx), (y |> IntToFloatEpx), (cwid |> IntToFloatEpx), (chei |> IntToFloatEpx),  // TODO: nice not to have conversions.
                fontDefinition.FontImageWithHostObject
            )) 
        
    

/// Draw text string in a given font, aligned in a given way with respect to a point.
let Text render (fontResource:FontID) hAlign vAlign (x:int<epx>) (y:int<epx>) message =
   
    let fontDefinition = FontFromID fontResource
    let cwid = fontDefinition.CharWidth
    let chei = fontDefinition.CharHeight

    let drawCharImage (index:int) (left:int) (top:int) =
        // TODO: Should algorithm use uint32 for the char index?
        DrawCharImageWithTopLeftAt render left top (uint32 index) fontDefinition
    
    // TODO: Constants? should be in the FontImageWithHostObject
    LayOutMonospaceFontTextString drawCharImage cwid chei (IntEpxToInt x) (IntEpxToInt y) message hAlign vAlign  



/// Draw a uint32 in a given font, aligned in a given way with respect to a point.
let Num render fontResource hAlign vAlign x y (value:uint32) =
    Text render fontResource hAlign vAlign x y (value.ToString())



/// Draw a float32 in a given font, aligned in a given way with respect to a point.
let Flo render fontResource hAlign vAlign x y (value:float32) =
    // TODO: the format string should be parameterized:
    Text render fontResource hAlign vAlign x y (sprintf "%.1f" value)



// Draw a repeated character starting from a given position extending for a given count.
// The direction is specified as integer pixel deltas.
let DrawRepeatedChar render (fontID:FontID) (dx:int<epx>) (dy:int<epx>) (charIndex:uint32) (startLeft:int<epx>) (startTop:int<epx>) numRepeats =

    let fontDefinition = FontFromID fontID

    let mutable x'    = startLeft
    let mutable y'    = startTop
    let mutable count = numRepeats

    while count > 0u do
        DrawCharImageWithTopLeftAt render (int x') (int y') charIndex fontDefinition  // TODO: We have inconsistent application of <epx> UOM because of LayOutMonospaceFontTextString
        count <- count - 1u
        x' <- x' + dx
        y' <- y' + dy

    (x', y')




// ---------------------------------------------------------------------------------------------------------
//  Drawing paragraph
// ---------------------------------------------------------------------------------------------------------

let Paragraph render fontResource hAlign vAlign (x:int<epx>) (y:int<epx>) (ydelta:int<epx>) messageList =

    messageList
        |> List.iteri (fun i message ->
            let y = y + ydelta * i
            Text render fontResource hAlign vAlign x y message)

