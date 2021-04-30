module ScreenGameTitle

open GameStateManagement
open DrawingFunctions
open ResourceIDs
open Geometry
open ImagesAndFonts
open InputEventData
open BeachBackgroundRenderer
open Time
open ScoreboardModel
open StaticResourceAccess
open ScoreHiScore

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

// TODO: Possibly implement fire button inhibit as like FreezeFrame

/// Intended to form a barrier against pressing FIRE 
/// repeatedly at the end of the Enter Your Name screen.
let private TimeBeforeResponding = 2.0F<seconds>

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private GameTitleScreenModel =
    {
        Scoreboard            : ScoreAndName list
        ScreenStartTime       : GameTime
        HiScore               : uint32
        ScoreboardMemo        : string list
        NextScreenConstructor : ScoreAndHiScore -> ErasedGameState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private IsFireButtonOperative model gameTime =

    let respondTime = model.ScreenStartTime + TimeBeforeResponding
    gameTime > respondTime

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderGameTitleScreen render model (gameTime:GameTime) =

    RenderBeachBackground render (gameTime / 4.0F)
    CentreImage render 160.0F<epx> 68.0F<epx> (ImageTitle |> ImageFromID)
    Paragraph render BlackFontID CentreAlign TopAlign 160<epx> 94<epx> 20<epx> model.ScoreboardMemo

    if IsFireButtonOperative model gameTime then
        Text render BlackFontID CentreAlign MiddleAlign 160<epx> 180<epx> "USE CURSOR KEYS ... Z TO FIRE"

    #if SHORT_PLAYTHROUGH
    Text render RedFontID CentreAlign MiddleAlign 160<epx> 10<epx> "WARNING  SHORT PLAY VERSION"
    #endif

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextGameTitleScreenState gameState keyStateGetter gameTime elapsed =

    let input = keyStateGetter |> DecodedInput
    let model = ModelFrom gameState

    if input.Fire.JustDown && IsFireButtonOperative model gameTime then
        model.NextScreenConstructor {Score=0u ; HiScore=model.HiScore}
    else
        Unchanged gameState
    
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewGameTitleScreen hiScore globalScoreboard nextConstructor gameTime =

    let titleScreenModel =
        {
            Scoreboard      = globalScoreboard
            HiScore         = hiScore
            ScreenStartTime = gameTime
            ScoreboardMemo  = ScoreboardText 30 globalScoreboard
            NextScreenConstructor = nextConstructor
        }

    NewGameState NextGameTitleScreenState RenderGameTitleScreen titleScreenModel

