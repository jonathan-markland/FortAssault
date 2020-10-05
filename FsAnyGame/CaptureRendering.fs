module CaptureRendering

open DrawingShapes



/// Calls the user-defined drawing function, supplying a render
/// function that records the resultant drawing in a list.
/// The drawing list is returned in REVERSE order.
let CaptureRenderingToReverseList userDefinedDrawingFunction =

    let mutable (drawingCommandsList:DrawingShapes list) = []

    let render command =
        drawingCommandsList <- command::drawingCommandsList

    userDefinedDrawingFunction render

    drawingCommandsList



/// Calls the user-defined drawing function, supplying a render
/// function that records the resultant drawing in a list.
/// The drawing list is returned.
let CaptureRenderingToList userDefinedDrawingFunction = 
    (CaptureRenderingToReverseList userDefinedDrawingFunction)
        |> List.rev


