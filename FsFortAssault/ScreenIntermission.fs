module ScreenIntermission

open Time
open ImagesAndFonts
open DrawingCommandsEx
open FontAlignment
open Geometry
open StoryboardChapterChange

let IntermissionDuration = 4.0F<seconds>

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type IntermissionScreenModel<'nextChapterType> =
    {
        EndTime                : float32<seconds>
        NextChapterConstructor : (float32<seconds> -> 'nextChapterType)
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderIntermissionScreen render model gameTime =
    Image1to1 render 0<wu> 0<wu> ImageIntermissionBackground.ImageID
    Text render YellowFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) (ScreenHeightInt / 2) "GET READY"

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewIntermissionScreenState gameTime chapterConstructor =
    {
        EndTime                = gameTime + IntermissionDuration
        NextChapterConstructor = chapterConstructor
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextIntermissionScreenState model input gameTime =

    if gameTime >= model.EndTime then
        GoToNextChapter1(model)
    else
        StayOnThisChapter1(model)

        