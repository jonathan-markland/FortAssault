module ScreenGameOver

open DrawingShapes
open DrawingFunctions
open ResourceIDs
open ScoreHiScore
open Geometry
open ImagesAndFonts
open ScreenHandler
open Time
open PacmanShared
open ResourceIDs
open TitleScreenShared

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type GameOverScreenModel =
    {
        ScoreAndHiScore : ScoreAndHiScore
        HiScoreText     : string
        WhereAfter      : ScoreAndHiScore -> float32<seconds> -> ErasedGameState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderGameOverScreen render (model:GameOverScreenModel) gameTime =

    Rectangle render 0<epx> 0<epx> ScreenWidthInt ScreenHeightInt (SolidColour(0x400000u))

    Text render GreyFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) (ScreenHeightInt / 3) "GAME OVER"
    Text render GreyFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) (ScreenHeightInt / 2) (model.HiScoreText)

    // TODO: Do we want a module for any of this artistic stuff, ie: on the title screen too?

    // let x50pc = 50 |> PercentOfScreenWidth
    // 
    // let y20pc = 20 |> PercentOfScreenHeight
    // let y50pc = 50 |> PercentOfScreenHeight
    // let y75pc = 75 |> PercentOfScreenHeight
    // 
    // DrawPacMan model.PacRightMemo
    // DrawPacMan model.PacLeftMemo 
    // 
    // DrawGhost  model.Ghost0Memo
    // DrawGhost  model.Ghost1Memo
    // DrawGhost  model.Ghost2Memo
    // DrawGhost  model.Ghost3Memo


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextGameOverScreenState gameState keyStateGetter gameTime _elapsed =

    //let model = ModelFrom gameState
    //    let input = keyStateGetter |> DecodedInput
    //
    //    if input.Fire.JustDown then
    //        model.WhereAfter model.ScoreAndHiScore gameTime
    //    else
        gameState |> Unchanged
    
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewGameOverScreen scoreAndHiScore whereAfter =

    let gameOverModel =
        {
            ScoreAndHiScore = scoreAndHiScore
            HiScoreText     = "HI SCORE   " + scoreAndHiScore.HiScore.ToString()
            WhereAfter      = whereAfter
        }

    NewGameState NextGameOverScreenState RenderGameOverScreen gameOverModel
