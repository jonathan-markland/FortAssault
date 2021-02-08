module ScreenGameTitle

open GameStateManagement
open DrawingFunctions
open ResourceIDs
open Geometry
open ImagesAndFonts
open Time
open ScoreboardModel
open StaticResourceAccess
open GamePlayScreenConstants

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

    let x50pc = 50 |> PercentOfScreenWidth
    let y1 = 30 |> PercentOfScreenHeight
    let y2 = 55 |> PercentOfScreenHeight
    let y3 = 65 |> PercentOfScreenHeight
    let y4 = 75 |> PercentOfScreenHeight

    let smallFont = FontFromID MissionIIFontID
    let bigFont = MagnifiedFont  6  4 6  smallFont

    TextX render bigFont   CentreAlign MiddleAlign x50pc y1 "MISSION II"
    TextX render smallFont CentreAlign MiddleAlign x50pc y2 "A RETRO REMAKE OF THE BBC MICRO CLASSIC"
    TextX render smallFont CentreAlign MiddleAlign x50pc y3 "CYBERTRON MISSION"
    TextX render smallFont CentreAlign MiddleAlign x50pc y4 "CONTROLS   CURSOR KEYS   Z FIRE"

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewGameTitleScreen globalScoreboard =

    let titleScreenModel =
        {
            Scoreboard      = globalScoreboard
            ScoreboardMemo  = ScoreboardText 24 globalScoreboard
        }

    NewGameState ModelNeverChanges RenderGameTitleScreen titleScreenModel


