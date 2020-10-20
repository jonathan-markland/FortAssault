module IntermissionCard

open Time
open DrawingFunctions
open Geometry
open ImagesAndFonts
open ScreenHandler

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private IntermissionCardModel =
    {
        EndTime          : float32<seconds>
        WhereToAfterCtor : float32<seconds> -> ErasedGameState
        BackgroundImage  : Image
        Font             : Font
        MessageText      : string
        ScreenWidthInt   : int<epx>
        ScreenHeightInt  : int<epx>
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderIntermissionCard render model _gameTime =

    Image1to1 render 0<epx> 0<epx> model.BackgroundImage

    TextX 
        render model.Font CentreAlign MiddleAlign 
        (model.ScreenWidthInt / 2) (model.ScreenHeightInt / 2) 
        model.MessageText

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextIntermissionCardState gameState _keyStateGetter gameTime _elapsed =

    let model = ModelFrom gameState

    if gameTime < model.EndTime then 
        Unchanged gameState
    else 
        model.WhereToAfterCtor gameTime

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let WithIntermission duration gameTime backgroundImage font messageText screenWidth screenHeight whereToAfter =

    let intermissionModel =
        {
            EndTime          = gameTime + duration
            WhereToAfterCtor = whereToAfter
            BackgroundImage  = backgroundImage
            Font             = font
            MessageText      = messageText
            ScreenWidthInt   = screenWidth
            ScreenHeightInt  = screenHeight
        }

    NewGameState NextIntermissionCardState RenderIntermissionCard intermissionModel

