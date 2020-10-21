﻿module EngineEntryPoint

open ScreenGameTitle
open ScreenGameOver
open ScreenInitialMap
open ScreenVictory
open ScreenPotentialEnterYourName
open ScreenMapBeforeBeachLanding

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

    // let highestScoreInInitialBoard = 
    //     HiScoreFromScoreboard gameGlobalState.GameScoreBoard
    // 
    // let fudge shs gameTime = NewGameOverScreen shs
    // 
    // let passage scoreAndHiScore gameTime = 
    //     NewVictoryScreen scoreAndHiScore fudge gameTime
    // 
    // NewGameTitleScreen 
    //     highestScoreInInitialBoard 
    //     gameGlobalState 
    //     (NewInitialMapScreen passage fudge)
    //     gameTime

    // let afterEntry updatedScoreboard gameTime =
    //     failwith ""
    // 
    // NewPotentialEnterYourNameScreen 
    //     {Score=10300u;HiScore=15000u}
    //     gameGlobalState.GameScoreBoard 
    //     afterEntry
    //     gameTime

    let afterEntry shs ships gameTime =
        failwith ""

    NewMapBeforeBeachLandingScreen 
        {Score=10300u ; HiScore=15000u}
        4u
        afterEntry

    






