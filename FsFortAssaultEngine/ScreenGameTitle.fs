module ScreenGameTitle

open DrawingFunctions
open ImagesAndFonts
open Geometry
open FontAlignment
open InputEventData
open BeachBackgroundRenderer
open Time
open FortAssaultGlobalState
open ScoreboardModel
open StaticResourceAccess

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Intended to form a barrier against pressing FIRE 
/// repeatedly at the end of the Enter Your Name screen.
let TimeBeforeResponding = 2.0F<seconds>

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type GameTitleScreenState =
    | GameTitleAwaitingFireButton
    | GameTitleScreenOver

type GameTitleScreenModel =
    {
        ScreenStartTime : float32<seconds>
        GameGlobalState : FortAssaultGlobalState
        HiScore         : uint32
        State           : GameTitleScreenState
        ScoreboardMemo  : string list
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

let NewGameTitleScreen hiScore gameGlobalState gameTime =
    {
        GameGlobalState = gameGlobalState
        HiScore         = hiScore
        State           = GameTitleAwaitingFireButton
        ScreenStartTime = gameTime
        ScoreboardMemo  = ScoreboardText 30 gameGlobalState.GameScoreBoard
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextGameTitleScreenState oldState input gameTime =

    if input.Fire.JustDown then

        if IsFireButtonOperative oldState gameTime then
            { oldState with State = GameTitleScreenOver }
        else
            oldState

    else
        oldState

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Query functions for Storyboard
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let StayOnTitleScreen state =
    match state.State with
        | GameTitleAwaitingFireButton -> true
        | GameTitleScreenOver -> false

    