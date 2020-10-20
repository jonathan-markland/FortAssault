module EngineEntryPoint

open ScreenGameTitle
open ScreenGameOver
open ScreenInitialMap
open ScoreboardModel
open FortAssaultGlobalState



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//   Creation of initial SpecificGameState
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Called only once when the game boots
let NewFortAssaultStoryboard (gameGlobalState:FortAssaultGlobalState) gameTime =

    // Todo:
    // #if SHORT_PLAYTHROUGH
    // let storyboard =
    //     Shortcut gameResources gameTime SkipToEnterYourName // RunGameNormally
    // #else
    // let storyboard =
    //     Shortcut gameResources gameTime RunGameNormally  // ** DO NOT CHANGE THIS ONE! : Define SHORT_PLAYTHROUGH and set the one above **
    // #endif

    let highestScoreInInitialBoard = 
        HiScoreFromScoreboard gameGlobalState.GameScoreBoard
    
    let fudge shs gameTime = NewGameOverScreen shs

    NewGameTitleScreen 
        highestScoreInInitialBoard 
        gameGlobalState 
        (NewInitialMapScreen fudge fudge)
        gameTime





