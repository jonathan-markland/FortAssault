module ScreenGameTitle

open GameStateManagement
open DrawingFunctions
open ResourceIDs
open Geometry
open ImagesAndFonts
open Time
open ScoreboardModel
open StaticResourceAccess

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private GameTitleScreenModel =
    {
        Scoreboard     : ScoreAndName list
        ScoreboardMemo : string list
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderGameTitleScreen render model (gameTime:float32<seconds>) =

    let backgroundImage = BackgroundImageID |> ImageFromID
    Image1to1 render 0<epx> 0<epx> backgroundImage

    (*
    let tilesImage = Level1ImageID |> ImageFromID

    let x50pc = 50 |> PercentOfScreenWidth
    let y20pc = 20 |> PercentOfScreenHeight
    let y50pc = 50 |> PercentOfScreenHeight
    let y75pc = 75 |> PercentOfScreenHeight

    let verticalSpacing = 16<epx>  // TODO: Twice the font height

    let pacAt   = DrawPacMan render tilesImage gameTime
    let ghostAt = DrawGhost render tilesImage gameTime 

    pacAt  model.PacRightMemo
    pacAt  model.PacLeftMemo 

    ghostAt  model.Ghost0Memo
    ghostAt  model.Ghost1Memo
    ghostAt  model.Ghost2Memo
    ghostAt  model.Ghost3Memo

    // TODO: Decided I didn't like this: let largeFont = (GreyFontID |> FontFromID) |> MagnifiedFont 1 2
    Text render GreyFontID CentreAlign MiddleAlign x50pc y20pc "PAC MAN"

    Paragraph render GreyFontID CentreAlign MiddleAlign x50pc y50pc verticalSpacing model.ScoreboardMemo

    Text render GreyFontID CentreAlign MiddleAlign x50pc y75pc "USE CURSOR KEYS ... Z TO START"
    *)

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewGameTitleScreen globalScoreboard =

    let titleScreenModel =
        {
            Scoreboard      = globalScoreboard
            ScoreboardMemo  = ScoreboardText 24 globalScoreboard
        }

    NewGameState ModelNeverChanges RenderGameTitleScreen titleScreenModel


