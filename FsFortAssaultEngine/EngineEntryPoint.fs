module EngineEntryPoint

open ScreenHandler
open ScreenGameTitle
open ScreenGameOver
open ScoreboardModel
open FortAssaultGlobalState



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//   Creation of initial SpecificGameState
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Called only once when the game boots
let NewFortAssaultStoryboard gameResources (gameGlobalState:FortAssaultGlobalState) gameTime =

    // Todo:
    // #if SHORT_PLAYTHROUGH
    // let storyboard =
    //     Shortcut gameResources gameTime SkipToEnterYourName // RunGameNormally
    // #else
    // let storyboard =
    //     Shortcut gameResources gameTime RunGameNormally  // ** DO NOT CHANGE THIS ONE! : Define SHORT_PLAYTHROUGH and set the one above **
    // #endif

    // let model = struct (storyboard , gameGlobalState)
    // model |> NewGameState NextStoryboardState RenderFortAssaultStoryboard

    // -------------------------------------------------------------------------------------------------

    let gameOverCtor scoreAndHiscore =
        let model = NewGameOverScreen scoreAndHiscore
        NewGameState NextGameOverScreenState RenderGameOverScreen model

    // -------------------------------------------------------------------------------------------------

    let highestScoreInInitialBoard = 
        HiScoreFromScoreboard gameGlobalState.GameScoreBoard
    
    let model = 
        NewGameTitleScreen 
            highestScoreInInitialBoard 
            gameGlobalState 
            gameOverCtor
            gameTime
    
    NewGameState NextGameTitleScreenState RenderGameTitleScreen model





