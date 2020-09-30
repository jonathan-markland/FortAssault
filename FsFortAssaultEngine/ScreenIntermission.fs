module ScreenIntermission

open Time
open ImagesAndFonts
open DrawingCommandsEx
open FontAlignment
open Geometry
open ResourceFileMetadata
open StaticResourceAccess


let IntermissionDuration = 4.0F<seconds>

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type IntermissionScreenModel<'nextChapterType> =
    {
        EndNow                 : bool
        EndTime                : float32<seconds>
        NextChapterConstructor : (float32<seconds> -> 'nextChapterType)
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderIntermissionScreen render model gameTime =
    Image1to1 render 0<epx> 0<epx> (ImageIntermissionBackground |> ImageFromID)
    Text render YellowFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) (ScreenHeightInt / 2) "GET READY"

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewIntermissionScreenState gameTime chapterConstructor =
    {
        EndNow                 = false
        EndTime                = gameTime + IntermissionDuration
        NextChapterConstructor = chapterConstructor
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextIntermissionScreenState model _input gameTime =
    if gameTime >= model.EndTime then 
        { model with EndNow = true } 
    else 
        model

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Query functions for Storyboard
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
        
let StayOnIntermission state =
    state.EndNow
