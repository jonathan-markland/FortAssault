module DrawingFunctions

open DrawingShapes
open Geometry
open Time
open ImagesAndFonts

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
let CentreImage render cx cy (imageWithHostObject:Image) =
    let (w,h) = imageWithHostObject |> ImageDimensions
    let left  = cx - ((w / 2) |> IntToFloatEpx)
    let top   = cy - ((h / 2) |> IntToFloatEpx)
    render (DrawStretchedImageWithTopLeftAt(left, top, imageWithHostObject, w, h))

/// Draw image centered about a point, without stretching.
let inline CentreImagePoint render { ptx=cx ; pty=cy } imageWithHostObject =
    CentreImage render cx cy imageWithHostObject

/// Draw image centered about a point, without stretching.
/// The image is one of a repeating animation collection.
let CycleImages render cx cy (imageArray:Image array) (fullCycleDuration:float32<seconds>) (elapsed:float32<seconds>) =
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
let SquareAroundPoint render left top (side:int<epx>) colour =  // TODO: Should be done in geometry to yield the square as a rectangle around the point, then draw the rectangle.
    let h = side / 2
    Rectangle render (left - h) (top - h) side side colour

// ---------------------------------------------------------------------------------------------------------
//  Drawing text
// ---------------------------------------------------------------------------------------------------------

/// Calculate the layout of an aligned monospace text string.
/// Calls 'drawCharImage charIndex LeftX TopY' for each character.
/// The chWidth/chHeight are the dimensions of a character at the target.
/// The font definition must consist of digits 0-9 then capitals A-Z.
let private LayOutMonospaceFontTextString drawCharImage chWidth chHeight x y message textHAlign textVAlign  =

    let measuredWidth (s:string) =
        s.Length * chWidth

    let mutable posx =
        match textHAlign with
            | LeftAlign   -> x
            | CentreAlign -> x - (message |> measuredWidth) / 2
            | RightAlign  -> x - (message |> measuredWidth)

    let posy =
        match textVAlign with
            | TopAlign    -> y
            | MiddleAlign -> y - (chHeight / 2)
            | BottomAlign -> y - chHeight

    message |> Seq.iter (fun ch -> 
        let write charIndex = drawCharImage charIndex posx posy
        if      ch >= '0' && ch <= '9' then write ((int ch) - 48)
        else if ch >= 'A' && ch <= 'Z' then write ((int ch) - 55)
        else if ch >= 'a' && ch <= 'z' then write ((int ch) - 87)
        else if ch = '.' then write 36
        else ()
        posx <- posx + chWidth
    )


let private DrawCharImageWithTopLeftAt render (x:int) (y:int) charIndex (fontDefinition:Font) =

    let srcCharWidth  = fontDefinition.SrcCharWidth
    let srcCharHeight = fontDefinition.SrcCharHeight
    
    let chx = (int charIndex) * srcCharWidth

    render (
        DrawSubImageStretchedToTarget (
            chx, 0, srcCharWidth, srcCharHeight,
            (x |> IntToFloatEpx), 
            (y |> IntToFloatEpx), 
            (fontDefinition.CharWidth  |> IntToIntEpx), 
            (fontDefinition.CharHeight |> IntToIntEpx),
            fontDefinition.FontImage)) 
        
    

/// Draw text string in a given font, aligned in a given way with respect to a point.
let TextX render (fontDefinition:Font) hAlign vAlign (x:int<epx>) (y:int<epx>) message =
   
    let drawCharImage (index:int) (left:int) (top:int) =
        // TODO: Should algorithm use uint32 for the char index?
        DrawCharImageWithTopLeftAt render left top (uint32 index) fontDefinition
    
    let cw = fontDefinition.CharWidth
    let ch = fontDefinition.CharHeight
    LayOutMonospaceFontTextString drawCharImage cw ch (IntEpxToInt x) (IntEpxToInt y) message hAlign vAlign  



/// Draw text string in a given font, aligned in a given way with respect to a point.
let Text render (fontID:FontID) hAlign vAlign (x:int<epx>) (y:int<epx>) message =
    TextX render (FontFromID fontID) hAlign vAlign x y message



/// Draw a uint32 in a given font, aligned in a given way with respect to a point.
let Num render fontID hAlign vAlign x y (value:uint32) =
    Text render fontID hAlign vAlign x y (value.ToString())



/// Draw a float32 in a given font, aligned in a given way with respect to a point.
let Flo render fontID hAlign vAlign x y (value:float32) =
    // TODO: the format string should be parameterized:
    Text render fontID hAlign vAlign x y (sprintf "%.1f" value)



// Draw a repeated character starting from a given position extending for a given count.
// The direction is specified as integer pixel deltas.
let DrawRepeatedCharX
    render (fontDefinition:Font) (dx:int<epx>) (dy:int<epx>) (charIndex:uint32) (startLeft:int<epx>) (startTop:int<epx>) numRepeats =

    let mutable x'    = startLeft
    let mutable y'    = startTop
    let mutable count = numRepeats

    while count > 0u do
        DrawCharImageWithTopLeftAt render (int x') (int y') charIndex fontDefinition  // TODO: We have inconsistent application of <epx> UOM because of LayOutMonospaceFontTextString
        count <- count - 1u
        x' <- x' + dx
        y' <- y' + dy

    (x', y')


// Draw a repeated character starting from a given position extending for a given count.
// The direction is specified as integer pixel deltas.
let DrawRepeatedChar render (fontID:FontID) (dx:int<epx>) (dy:int<epx>) (charIndex:uint32) (startLeft:int<epx>) (startTop:int<epx>) numRepeats =
    DrawRepeatedCharX render (fontID |> FontFromID) dx dy charIndex startLeft startTop numRepeats



// ---------------------------------------------------------------------------------------------------------
//  Drawing paragraph
// ---------------------------------------------------------------------------------------------------------

let ParagraphX render fontDefinition hAlign vAlign (x:int<epx>) (y:int<epx>) (ydelta:int<epx>) messageList =

    match messageList with
        | [] -> ()
        | _  ->

            let chei   = fontDefinition.CharHeight |> IntToIntEpx
            let ydelta = max chei ydelta

            let verticalSpan = (messageList.Length - 1) * ydelta + chei

            let y =
                match vAlign with
                    | TopAlign    -> y
                    | MiddleAlign -> y - verticalSpan / 2
                    | BottomAlign -> y - verticalSpan

            messageList
                |> List.iteri (fun i message ->
                    let y = y + ydelta * i
                    TextX render fontDefinition hAlign TopAlign x y message)


    
let Paragraph render fontResource hAlign vAlign (x:int<epx>) (y:int<epx>) (ydelta:int<epx>) messageList =
    ParagraphX render (FontFromID fontResource) hAlign vAlign (x:int<epx>) (y:int<epx>) (ydelta:int<epx>) messageList


