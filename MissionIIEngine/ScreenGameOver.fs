module ScreenGameOver

open GameStateManagement
open DrawingFunctions
open ResourceIDs
open Geometry
open ImagesAndFonts
open Time
open StaticResourceAccess
open GamePlayScreenConstants
open ScoreHiScore

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private GameOverScreenModel =
    {
        ScoreMemo : string
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderGameOverScreen render model (gameTime:float32<seconds>) =

    let backgroundImage = Background2ImageID |> ImageFromID
    Image1to1 render 0<epx> 0<epx> backgroundImage

    let x50pc = 50 |> PercentOfScreenWidth
    let y1 = 40 |> PercentOfScreenHeight
    let y2 = 60 |> PercentOfScreenHeight

    let smallFont = FontFromID MissionIIFontID
    let bigFont = MagnifiedFont  6  3 5  smallFont

    TextX render bigFont     CentreAlign MiddleAlign x50pc y1 "GAME OVER"
    TextX render smallFont   CentreAlign MiddleAlign x50pc y2 model.ScoreMemo

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewGameOverScreen scoreAndHiScore gameTime =

    let {
            Score   = score
            HiScore = _
        } = scoreAndHiScore

    let model =
        {
            ScoreMemo = sprintf "SCORE %d" score
        }

    NewGameState ModelNeverChanges RenderGameOverScreen model


