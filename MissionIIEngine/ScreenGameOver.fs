module ScreenGameOver

(*

open DrawingFunctions
open ResourceIDs
open ScoreHiScore
open Geometry
open ImagesAndFonts
open GameStateManagement
open Directions
open PacmanShared
open TitleScreenShared
open StaticResourceAccess

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private GameOverScreenModel =
    {
        ScoreAndHiScore : ScoreAndHiScore
        ScoreText       : string
        HiScoreText     : string
        PacLeftMemo     : TitleScreenPacmanState
        PacRightMemo    : TitleScreenPacmanState
        Ghost0Memo      : TitleScreenGhostState
        Ghost1Memo      : TitleScreenGhostState
        Ghost2Memo      : TitleScreenGhostState
        Ghost3Memo      : TitleScreenGhostState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderGameOverScreen render (model:GameOverScreenModel) gameTime =

    let backgroundImage = Background3ImageID |> ImageFromID
    Image1to1 render 0<epx> 0<epx> backgroundImage

    let msgAt percent message =
        Text render GreyFontID CentreAlign MiddleAlign (ScreenWidthInt / 2) (percent |> PercentOfScreenHeight) message

    let tilesImage = 
        Level1ImageID |> ImageFromID

    let pacAt = 
        DrawPacMan render tilesImage gameTime

    let ghostAt = 
        DrawGhost render tilesImage gameTime 

    msgAt  20 "PAC MAN"
    msgAt  30 "GAME OVER"
    msgAt  45 model.ScoreText
    msgAt  55 model.HiScoreText

    pacAt  model.PacRightMemo
    pacAt  model.PacLeftMemo 

    ghostAt  model.Ghost0Memo
    ghostAt  model.Ghost1Memo
    ghostAt  model.Ghost2Memo
    ghostAt  model.Ghost3Memo

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewGameOverScreen scoreAndHiScore =

    let gameOverModel =
        {
            ScoreAndHiScore = scoreAndHiScore
            ScoreText       = "SCORE   " + scoreAndHiScore.Score.ToString()
            HiScoreText     = "HI SCORE   " + scoreAndHiScore.HiScore.ToString()
            PacLeftMemo     = TitleScreenPac FacingRight 20 50 
            PacRightMemo    = TitleScreenPac FacingLeft  80 50
            Ghost0Memo      = TitleScreenGhost (GhostNumber 0) 20 75
            Ghost1Memo      = TitleScreenGhost (GhostNumber 1) 40 75
            Ghost2Memo      = TitleScreenGhost (GhostNumber 2) 60 75
            Ghost3Memo      = TitleScreenGhost (GhostNumber 3) 80 75
        }

    NewGameState ModelNeverChanges RenderGameOverScreen gameOverModel

    *)