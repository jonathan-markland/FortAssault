module ScreenGameTitle

open ScreenHandler
open DrawingFunctions
open ResourceIDs
open Geometry
open ImagesAndFonts
open Time
open ScoreboardModel
open StaticResourceAccess
open InterruptableVideo
open DrawingShared
open Input

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private GameTitleScreenModel =
    {
        Scoreboard     : ScoreAndName list
        ScoreboardMemo : string list
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderGameTitleScreen render model (gameTime:float32<seconds>) =

    let backgroundImage = Background2ImageID |> ImageFromID
    Image1to1 render 0<epx> 0<epx> backgroundImage

    let tilesImage = Level1ImageID |> ImageFromID

    let inline percentOfScreenWidth  x = (ScreenWidthInt * x) / 100
    let inline percentOfScreenHeight x = (ScreenHeightInt * x) / 100

    let x20pc = 20 |> percentOfScreenWidth
    let x40pc = 40 |> percentOfScreenWidth
    let x50pc = 50 |> percentOfScreenWidth
    let x60pc = 60 |> percentOfScreenWidth
    let x80pc = 80 |> percentOfScreenWidth

    let y20pc = 20 |> percentOfScreenHeight
    let y50pc = 50 |> percentOfScreenHeight
    let y75pc = 75 |> percentOfScreenHeight
    let y90pc = 90 |> percentOfScreenHeight

    let verticalSpacing = 16<epx>

    let pillMode = false

    DrawPacMan render tilesImage x20pc y20pc FacingRight pillMode gameTime
    DrawPacMan render tilesImage x80pc y20pc FacingLeft  pillMode gameTime

    DrawGhost render tilesImage x20pc y90pc (GhostNumber 0) gameTime
    DrawGhost render tilesImage x40pc y90pc (GhostNumber 1) gameTime
    DrawGhost render tilesImage x60pc y90pc (GhostNumber 2) gameTime
    DrawGhost render tilesImage x80pc y90pc (GhostNumber 3) gameTime

    Text render GreyFontID CentreAlign MiddleAlign x50pc y20pc "PAC MAN"

    Paragraph render GreyFontID CentreAlign MiddleAlign x50pc y50pc verticalSpacing model.ScoreboardMemo

    Text render GreyFontID CentreAlign MiddleAlign x50pc y75pc "USE CURSOR KEYS ... Z TO START"

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextGameTitleScreenState gameState keyStateGetter gameTime elapsed =
    Unchanged gameState
    
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewGameTitleScreen globalScoreboard nextConstructor =

    let titleScreenModel =
        {
            Scoreboard      = globalScoreboard
            ScoreboardMemo  = ScoreboardText 24 globalScoreboard
        }

    NewGameState NextGameTitleScreenState RenderGameTitleScreen titleScreenModel
        |> AsInterruptableVideoThen nextConstructor (WebBrowserKeyCode 90)


