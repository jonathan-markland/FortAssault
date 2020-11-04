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
open PacmanShared
open Input

// TODO: Possiblity for library?  For convenience?
let inline PercentOfScreenWidth  x = (ScreenWidthInt * x) / 100
let inline PercentOfScreenHeight x = (ScreenHeightInt * x) / 100

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private TitleScreenPacmanState =
    {
        pos       : Point<int<epx>>
        direction : FacingDirection
    }

type private TitleScreenGhostState =
    {
        pos    : Point<int<epx>>
        number : GhostNumber
    }

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

    let verticalSpacing = 16<epx>

    let DrawPacMan pacmanState =
        let pillMode = false
        let { pos=pos ; direction=facingDirection } = pacmanState
        DrawPacMan render tilesImage pos facingDirection pillMode gameTime

    let DrawGhost ghostState =
        let { pos=pos ; number=ghostNumber } = ghostState
        DrawGhost render tilesImage pos ghostNumber GhostNormal gameTime

    DrawPacMan model.PacRightMemo
    DrawPacMan model.PacLeftMemo 

    DrawGhost  model.Ghost0Memo
    DrawGhost  model.Ghost1Memo
    DrawGhost  model.Ghost2Memo
    DrawGhost  model.Ghost3Memo

    Text render GreyFontID CentreAlign MiddleAlign x50pc y20pc "PAC MAN"

    Paragraph render GreyFontID CentreAlign MiddleAlign x50pc y50pc verticalSpacing model.ScoreboardMemo

    Text render GreyFontID CentreAlign MiddleAlign x50pc y75pc "USE CURSOR KEYS ... Z TO START"

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextGameTitleScreenState gameState keyStateGetter gameTime elapsed =
    Unchanged gameState
    
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private TitleScreenPac facing percentX percentY =

    let cx = percentX |> PercentOfScreenWidth
    let cy = percentY |> PercentOfScreenHeight
    let h  = TileSide / 2

    { 
        pos = { ptx = cx - h ; pty = cy - h }
        direction = facing
    }

let private TitleScreenGhost ghostNumber percentX percentY =

    let cx = percentX |> PercentOfScreenWidth
    let cy = percentY |> PercentOfScreenHeight
    let h  = TileSide / 2

    { 
        pos = { ptx = cx - h ; pty = cy - h }
        number = ghostNumber
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewGameTitleScreen globalScoreboard nextConstructor =

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

    NewGameState NextGameTitleScreenState RenderGameTitleScreen titleScreenModel
        |> AsInterruptableVideoThen nextConstructor (WebBrowserKeyCode 90)  // TODO: Fire button constant?


