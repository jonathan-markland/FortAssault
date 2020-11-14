module ScreenGameOver

open DrawingShapes
open DrawingFunctions
open ResourceIDs
open ScoreHiScore
open Geometry
open ImagesAndFonts
open ScreenHandler
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

    msgAt  10 "PAC MAN"
    msgAt  20 model.ScoreText
    msgAt  40 "GAME OVER"
    msgAt  90 model.HiScoreText

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
            PacLeftMemo     = TitleScreenPac FacingRight 20 40 
            PacRightMemo    = TitleScreenPac FacingLeft  80 40
            Ghost0Memo      = TitleScreenGhost (GhostNumber 0) 20 60
            Ghost1Memo      = TitleScreenGhost (GhostNumber 1) 40 60
            Ghost2Memo      = TitleScreenGhost (GhostNumber 2) 60 60
            Ghost3Memo      = TitleScreenGhost (GhostNumber 3) 80 60
        }

    NewGameState ModelNeverChanges RenderGameOverScreen gameOverModel
