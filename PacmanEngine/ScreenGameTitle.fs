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

type private GameTitleScreenModel =
    {
        Scoreboard     : ScoreAndName list
        ScoreboardMemo : string list
        PacLeftMemo    : PacmanState
        PacRightMemo   : PacmanState
        Ghost0Memo     : GhostState
        Ghost1Memo     : GhostState
        Ghost2Memo     : GhostState
        Ghost3Memo     : GhostState
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

    let pillMode = false

    let (originx,originy) = (0<epx> , 0<epx>)

    DrawPacMan render tilesImage originx originy model.PacRightMemo false gameTime
    DrawPacMan render tilesImage originx originy model.PacLeftMemo  false gameTime

    DrawGhost render tilesImage originx originy model.Ghost0Memo gameTime
    DrawGhost render tilesImage originx originy model.Ghost1Memo gameTime
    DrawGhost render tilesImage originx originy model.Ghost2Memo gameTime
    DrawGhost render tilesImage originx originy model.Ghost3Memo gameTime

    Text render GreyFontID CentreAlign MiddleAlign x50pc y20pc "PAC MAN"

    Paragraph render GreyFontID CentreAlign MiddleAlign x50pc y50pc verticalSpacing model.ScoreboardMemo

    Text render GreyFontID CentreAlign MiddleAlign x50pc y75pc "USE CURSOR KEYS ... Z TO START"

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextGameTitleScreenState gameState keyStateGetter gameTime elapsed =
    Unchanged gameState
    
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let TitleScreenPac facing percentX percentY =

    let cx = percentX |> PercentOfScreenWidth
    let cy = percentY |> PercentOfScreenHeight

    let h = TileSide / 2

    { 
        PacPosition = { ptix=cx-h ; ptiy=cy-h }
        PacState2 = 
            {
                PacMode            = PacAlive
                PacFacingDirection = facing
                LivesLeft          = 3
            }
    }

let TitleScreenGhost ghostNumber percentX percentY =

    let cx = percentX |> PercentOfScreenWidth
    let cy = percentY |> PercentOfScreenHeight

    let h = TileSide / 2

    let pos = { ptix=cx-h ; ptiy=cy-h } 

    { 
        GhostPosition = pos
        GhostState2 = 
            { 
                GhostNumber       = ghostNumber
                GhostMode         = GhostNormal
                GhostHomePosition = pos
            } 
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


