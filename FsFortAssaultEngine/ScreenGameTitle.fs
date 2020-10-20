module ScreenGameTitle

open ScreenHandler
open DrawingFunctions
open ResourceIDs
open Geometry
open ImagesAndFonts
open InputEventData
open BeachBackgroundRenderer
open Time
open FortAssaultGlobalState
open FortAssaultGameResources
open ScoreboardModel
open StaticResourceAccess
open ScoreHiScore

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Intended to form a barrier against pressing FIRE 
/// repeatedly at the end of the Enter Your Name screen.
let TimeBeforeResponding = 2.0F<seconds>

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type GameTitleScreenModel =
    {
        GameGlobalState       : FortAssaultGlobalState
        ScreenStartTime       : float32<seconds>
        HiScore               : uint32
        ScoreboardMemo        : string list
        NextScreenConstructor : ScoreAndHiScore -> ErasedGameState<FortAssaultGameResources>
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let IsFireButtonOperative oldState gameTime =

    let respondTime = oldState.ScreenStartTime + TimeBeforeResponding
    gameTime > respondTime

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderGameTitleScreen render model (gameTime:float32<seconds>) =

    RenderBeachBackground render (gameTime / 4.0F)
    CentreImage render 160.0F<epx> 68.0F<epx> (ImageTitle |> ImageFromID)
    Paragraph render BlackFontID CentreAlign TopAlign 160<epx> 94<epx> 20<epx> model.ScoreboardMemo

    if IsFireButtonOperative model gameTime then
        Text render BlackFontID CentreAlign MiddleAlign 160<epx> 180<epx> "USE CURSOR KEYS ... Z TO FIRE"

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewGameTitleScreen hiScore gameGlobalState nextConstructor gameTime =
    {
        GameGlobalState = gameGlobalState
        HiScore         = hiScore
        ScreenStartTime = gameTime
        ScoreboardMemo  = ScoreboardText 30 gameGlobalState.GameScoreBoard
        NextScreenConstructor = nextConstructor
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextGameTitleScreenState staticGameResources gameState keyStateGetter gameTime elapsed =

    let input = keyStateGetter |> DecodedInput
    let model = ModelFrom gameState

    if input.Fire.JustDown && IsFireButtonOperative model gameTime then
        model.NextScreenConstructor {Score=100u ; HiScore=10000u}
    else
        Unchanged gameState
    