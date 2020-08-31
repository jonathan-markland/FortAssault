module ScreenGameTitle

open DrawingCommandsEx
open ImagesAndFonts
open Geometry
open FontAlignment
open InputEventData
open StoryboardChapterChange
open BeachBackgroundRenderer
open Time
open GameGlobalState
open ScoreboardModel

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
        GameGlobalState : GameGlobalState
        HiScore         : uint32
        State           : GameTitleScreenState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderGameTitleScreen render model (gameTime:float32<seconds>) =

    RenderBeachBackground render (gameTime / 4.0F)
    CentreImage render 160.0F<epx> 68.0F<epx> ImageTitle

    let scoreboardText = ScoreboardText 30 model.GameGlobalState.GameScoreBoard  // TODO: memoize in the model?
    Paragraph render BlackFontID CentreAlign MiddleAlign 160<epx> 94<epx> 20<epx> scoreboardText

    Text render BlackFontID CentreAlign MiddleAlign 160<epx> 180<epx> "USE CURSOR KEYS ... Z TO FIRE"

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewGameTitleScreen hiScore gameGlobalState gameTime =
    {
        GameGlobalState = gameGlobalState
        HiScore              = hiScore
        State                = GameTitleAwaitingFireButton
        ScreenStartTime      = gameTime
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextGameTitleScreenState oldState input gameTime =

    match oldState.State with
        
        | GameTitleAwaitingFireButton ->

            if input.Fire.JustDown then

                let respondTime = oldState.ScreenStartTime + TimeBeforeResponding

                if gameTime > respondTime then
                    GoToNextChapter1({ oldState with State = GameTitleScreenOver })

                else
                    StayOnThisChapter1(oldState)

            else
                StayOnThisChapter1(oldState)

        | GameTitleScreenOver ->
            StayOnThisChapter1(oldState)
