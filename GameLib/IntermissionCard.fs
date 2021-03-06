﻿module IntermissionCard

open DrawingFunctions
open Geometry
open ImagesAndFonts
open GameStateManagement

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private IntermissionCardModel =
    {
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

let NewIntermissionCard backgroundImage font messageText screenWidth screenHeight =

    let intermissionModel =
        {
            BackgroundImage  = backgroundImage
            Font             = font
            MessageText      = messageText
            ScreenWidthInt   = screenWidth
            ScreenHeightInt  = screenHeight
        }

    NewGameState ModelNeverChanges RenderIntermissionCard intermissionModel

