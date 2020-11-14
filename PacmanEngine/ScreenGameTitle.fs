module ScreenGameTitle

open ScreenHandler
open DrawingFunctions
open ResourceIDs
open Geometry
open ImagesAndFonts
open Time
open ScoreboardModel
open StaticResourceAccess
open PacmanShared
open Directions
open TitleScreenShared

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private GameTitleScreenModel =
    {
        Scoreboard     : ScoreAndName list
        ScoreboardMemo : string list
        PacLeftMemo    : TitleScreenPacmanState
        PacRightMemo   : TitleScreenPacmanState
        Ghost0Memo     : TitleScreenGhostState
        Ghost1Memo     : TitleScreenGhostState
        Ghost2Memo     : TitleScreenGhostState
        Ghost3Memo     : TitleScreenGhostState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderGameTitleScreen render model (gameTime:float32<seconds>) =

    let backgroundImage = Background2ImageID |> ImageFromID
    Image1to1 render 0<epx> 0<epx> backgroundImage

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

    Text render GreyFontID CentreAlign MiddleAlign x50pc y20pc "PAC MAN"

    Paragraph render GreyFontID CentreAlign MiddleAlign x50pc y50pc verticalSpacing model.ScoreboardMemo

    Text render GreyFontID CentreAlign MiddleAlign x50pc y75pc "USE CURSOR KEYS ... Z TO START"

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewGameTitleScreen globalScoreboard =

    let titleScreenModel =
        {
            Scoreboard      = globalScoreboard
            ScoreboardMemo  = ScoreboardText 24 globalScoreboard
            PacLeftMemo     = TitleScreenPac FacingRight 20 20 
            PacRightMemo    = TitleScreenPac FacingLeft  80 20
            Ghost0Memo      = TitleScreenGhost (GhostNumber 0) 20 90
            Ghost1Memo      = TitleScreenGhost (GhostNumber 1) 40 90
            Ghost2Memo      = TitleScreenGhost (GhostNumber 2) 60 90
            Ghost3Memo      = TitleScreenGhost (GhostNumber 3) 80 90
        }

    NewGameState ModelNeverChanges RenderGameTitleScreen titleScreenModel


