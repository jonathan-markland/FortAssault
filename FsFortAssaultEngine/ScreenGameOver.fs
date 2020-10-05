module ScreenGameOver

open DrawingShapes
open DrawingFunctions
open ImagesAndFonts
open ScoreHiScore
open FontAlignment
open Geometry
open InputEventData
open ResourceFileMetadata
open StaticResourceAccess

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type GameOverScreenModel =
    {
        ScoreAndHiScore : ScoreAndHiScore
        HiScoreText     : string
        RestartNow      : bool
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderGameOverScreen render (model:GameOverScreenModel) gameTime =
    Rectangle render 0<epx> 0<epx> ScreenWidthInt ScreenHeightInt (SolidColour(0x000000u))
    Text render YellowFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) (ScreenHeightInt / 2) "GAME OVER"
    Text render BlueFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) (125<epx>) (model.HiScoreText)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewGameOverScreen scoreAndHiScore =
    {
        ScoreAndHiScore  = scoreAndHiScore
        HiScoreText      = "HI SCORE   " + scoreAndHiScore.HiScore.ToString()
        RestartNow       = false
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextGameOverScreenState oldState input _gameTime =

    if input.Fire.JustDown then
        { oldState with RestartNow = true }
    else
        oldState
        
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Query functions for Storyboard
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let StayOnGameOverScreen state =
    not (state.RestartNow)
