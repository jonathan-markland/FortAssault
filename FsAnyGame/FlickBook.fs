module FlickBook

open Algorithm
open Time
open Mechanics
open DrawingFunctions
open ImagesAndFonts


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Flickbook Types
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type FlickBookVisibility = Visible | Hidden

type FlickBookType =
    {
        FlickBookDuration      : float32<seconds>
        FlickBookImages        : ImageWithHostObject[]   // Array used so we can calculate an index from a time offset in O(1).
        VisibilityBeforeStart  : FlickBookVisibility
        VisibilityAfterEnd     : FlickBookVisibility
    }

type FlickBookInstance =
    {
        FlickBookType            : FlickBookType
        FlickBookStartTime       : float32<seconds>
        FlickBookMechanicsObject : MechanicsObjectModel
    }



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Flickbook functions
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let FlickBookStartTimeOf flickBook =
    flickBook.FlickBookStartTime

let FlickBookHeadItemStartTime flickBookList =
    match flickBookList with 
        | [] -> None
        | head::_ -> Some(head.FlickBookStartTime)

/// Asks whether a FlickBookInstance is still in play at the given game time.
let FlickBookInstanceStillInPlay gameTime flickBook =
    flickBook.FlickBookMechanicsObject |> IsMOMStillInPlayAt gameTime

/// Returns the position of the flickbook instance at the time given,
/// irrespective of the visibility rules defined for the flick book type.
let FlickBookPositionAtTime gameTime flickBook =
    flickBook.FlickBookMechanicsObject |> MOMPositionAt gameTime

/// Return a new generic list with all records removed where the record has a completed flickbook in
/// one of its fields.  The innerFlickBook function obtains the flickbook from the field.
let WithCompletedBuriedFlickbooksRemoved gameTime innerFlickBook genericList =
    genericList |> PlanetSavingListFilter (fun usersObject -> FlickBookInstanceStillInPlay gameTime (usersObject |> innerFlickBook))

/// Return a new list of FlickBookInstances, removing all ones that have completed.
let WithCompletedFlickbooksRemoved gameTime flickBookList =
    WithCompletedBuriedFlickbooksRemoved gameTime id flickBookList



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Flickbook drawing functions
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Draw a flick book instance for the given game time.
let DrawFlickbookInstance render flickBookInstance gameTime =

    let 
        {
            FlickBookType = flickBookType
            FlickBookStartTime = animStartTime
            FlickBookMechanicsObject = mom
        } = flickBookInstance

    let
        { 
            FlickBookDuration = duration
            FlickBookImages = imageArray
            VisibilityBeforeStart = startVis
            VisibilityAfterEnd = endVis 
        } = flickBookType
    

    let elapsed   = gameTime - animStartTime
    let numImages = imageArray.Length

    match mom.PositionGetter gameTime with

        | MOMYetToAppear ->

            if startVis = Visible && numImages > 0 then
                let pos = mom.StartPosition
                CentreImage render pos.ptx pos.pty (imageArray.[0])

        | MOMVisibleAtPosition (pos) ->

            let timePerImage = duration / (float32 numImages)
            let index = abs (int (elapsed / timePerImage))
            if index < numImages then
                CentreImage render pos.ptx pos.pty (imageArray.[index])

        | MOMDisappeared ->

            if endVis = Visible && numImages > 0 then
                let pos = mom.FinalPosition
                CentreImage render pos.ptx pos.pty (imageArray.[imageArray.Length - 1])



/// For each generic record in a list, obtain a flickbook from a field, and draw it.
/// The innerFlickBook function must dig the flickbook out of the record.
let DrawBuriedFlickbookInstanceList render innerFlickBook genericList gameTime =
    genericList |> List.iter (fun userObject -> DrawFlickbookInstance render (userObject |> innerFlickBook) gameTime)



/// Draw a list of FlickbookInstance objects.
let DrawFlickbookInstanceList render flickBookList gameTime =
    DrawBuriedFlickbookInstanceList render id flickBookList gameTime


