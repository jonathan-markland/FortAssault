module ScreenGameOver

open DrawingShapes
open DrawingFunctions
open ResourceIDs
open ScoreHiScore
open Geometry
open InputEventData
open ImagesAndFonts
open ScreenHandler

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

let NextGameOverScreenState gameState keyStateGetter gameTime elapsed =

    let model = ModelFrom gameState
    let input = keyStateGetter |> DecodedInput

    if input.Fire.JustDown then
        { model with RestartNow = true } |> ReplacesModelIn gameState
    else
        gameState |> Unchanged
    
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewGameOverScreen scoreAndHiScore =

    let gameOverModel =
        {
            ScoreAndHiScore  = scoreAndHiScore
            HiScoreText      = "HI SCORE   " + scoreAndHiScore.HiScore.ToString()
            RestartNow       = false
        }

    NewGameState NextGameOverScreenState RenderGameOverScreen gameOverModel


