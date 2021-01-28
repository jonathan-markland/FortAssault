module BlankScreen

open Geometry
open GameStateManagement
open DrawingShapes

type BlankScreenModel =
    {
        ScreenColour : SolidColour
    }

let NewBlankScreen screenWidth screenHeight colour =

    let renderBlankScreen render (model:BlankScreenModel) gameTime =
        render (DrawFilledRectangle (0<epx>, 0<epx>, screenWidth, screenHeight, model.ScreenColour))

    let blankScreenModel = { ScreenColour = colour }
    NewGameState ModelNeverChanges renderBlankScreen blankScreenModel
