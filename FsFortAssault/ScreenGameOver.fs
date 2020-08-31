module ScreenGameOver

open Time
open DrawingCommands
open DrawingCommandsEx
open ImagesAndFonts
open ScoreHiScore
open FontAlignment
open Geometry
open InputEventData
open StoryboardChapters

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type GameOverScreenModel =
    {
        ScoreAndHiScore : ScoreAndHiScore
        HiScoreText     : string
        RestartNow      : bool
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderGameOverScreen render (model:GameOverScreenModel) gameTime =
    Rectangle render 0<wu> 0<wu> ScreenWidthInt ScreenHeightInt (SolidColour(0x000000u))
    Text render YellowFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) (ScreenHeightInt / 2) "GAME OVER"
    Text render BlueFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) (125<wu>) (model.HiScoreText)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewGameOverScreen scoreAndHiScore =
    {
        ScoreAndHiScore  = scoreAndHiScore
        HiScoreText      = "HI SCORE   " + scoreAndHiScore.HiScore.ToString()
        RestartNow       = false
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextGameOverScreenState oldState input gameTime =

    let newModel =
        if input.Fire.JustDown then
            { oldState with RestartNow = true }
        else
            oldState

    if newModel.RestartNow then
        GoToNextChapter1(newModel)
    else
        StayOnThisChapter1(newModel)
        