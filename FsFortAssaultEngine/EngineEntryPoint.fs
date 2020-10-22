module EngineEntryPoint

open ScreenGameTitle
open ScreenGameOver
open ScreenInitialMap
open ScreenVictory
open ScreenPotentialEnterYourName
open ScreenMapBeforeBeachLanding
open ScreenMapPostPassage
open ScreenTankBattle
open ScreenSecretPassage
open ScreenAirBattle
open FinalBossAndTankBattleShared
open TankMapFileLoader

open ScoreHiScore
open ScoreboardModel
open FortAssaultGlobalState
open Rules



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
    // let scoreAndHiScore = 
    //     {Score=10300u ; HiScore=15000u}
    // 
    // let fudgeGameOver _gameTime =
    //     NewGameOverScreen scoreAndHiScore
    // 
    // let fudgeGameOver2 _shs _gameTime =
    //     NewGameOverScreen scoreAndHiScore
    // 
    // let passage gameTime = 
    //     NewVictoryScreen scoreAndHiScore fudgeGameOver2 gameTime
    // 
    // NewGameTitleScreen 
    //     highestScoreInInitialBoard 
    //     gameGlobalState 
    //     (NewInitialMapScreen passage fudgeGameOver)
    //     gameTime

    // let afterEntry updatedScoreboard gameTime =
    //     failwith ""
    // 
    // NewPotentialEnterYourNameScreen 
    //     {Score=10300u;HiScore=15000u}
    //     gameGlobalState.GameScoreBoard 
    //     afterEntry
    //     gameTime

    // let afterEntry gameTime =
    //     failwith ""
    // 
    // NewMapBeforeBeachLandingScreen 
    //     {Score=10300u ; HiScore=15000u}
    //     4u
    //     afterEntry

    // let afterEntry gameTime =
    //     failwith ""
    // 
    // NewMapPostPassageScreen
    //     {Score=10300u ; HiScore=15000u}
    //     4u
    //     afterEntry

    // let fudgeGameOver scoreAndHiScore gameTime =
    //     NewGameOverScreen scoreAndHiScore
    // 
    // let fudgeCourseComplete tanksRemaining shs gameTime =
    //     failwith ""
    // 
    // let finalBossTargets = NewFinalBossAndTankBattleData ()
    // 
    // match LoadTankBattleSequences () with
    //     | Error _ -> 
    //         failwith ""
    // 
    //     | Ok tankMaps ->
    //         NewTankBattleScreen
    //             {Score=10300u ; HiScore=15000u}
    //             5u
    //             finalBossTargets
    //             tankMaps
    //             fudgeGameOver
    //             fudgeCourseComplete
    //             gameTime

    // let fudgeGameOver scoreAndHiScore gameTime =
    //     NewGameOverScreen scoreAndHiScore
    // 
    // let fudgeCourseComplete shipsThrough shs gameTime =
    //     failwith ""
    // 
    // NewSecretPassageScreen 
    //     {Score=10300u ; HiScore=15000u}
    //     3u
    //     fudgeGameOver
    //     fudgeCourseComplete
    //     gameTime

    // let fudgeGameOver scoreAndHiScore gameTime =
    //     NewGameOverScreen scoreAndHiScore
    // 
    // let fudgeCourseComplete shipsThrough shs gameTime =
    //     failwith ""
    // 
    // NewAirBattleScreen
    //     StrongEnemy
    //     {Score=10300u ; HiScore=15000u}
    //     3u
    //     fudgeGameOver
    //     fudgeCourseComplete
    //     gameTime





    

