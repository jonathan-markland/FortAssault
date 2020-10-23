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
open ScreenSeaBattle
open FinalBossAndTankBattleShared
open TankMapFileLoader
open ScreenFinalBoss

open ScoreHiScore
open ScoreboardModel
open FortAssaultGlobalState
open Rules



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//   Creation of initial SpecificGameState
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Called only once when the game boots

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
    // let whenGameOver _gameTime =
    //     NewGameOverScreen scoreAndHiScore
    // 
    // let whenGameOver2 _shs _gameTime =
    //     NewGameOverScreen scoreAndHiScore
    // 
    // let passage gameTime = 
    //     NewVictoryScreen scoreAndHiScore whenGameOver2 gameTime
    // 
    // NewGameTitleScreen 
    //     highestScoreInInitialBoard 
    //     gameGlobalState 
    //     (NewInitialMapScreen passage whenGameOver)
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


    // let whenGameOver scoreAndHiScore gameTime =
    //     NewGameOverScreen scoreAndHiScore
    // 
    // let whenCourseComplete shipsThrough shs gameTime =
    //     failwith ""
    // 
    // NewSecretPassageScreen 
    //     {Score=10300u ; HiScore=15000u}
    //     3u
    //     whenGameOver
    //     whenCourseComplete
    //     gameTime

    // let whenGameOver scoreAndHiScore gameTime =
    //     NewGameOverScreen scoreAndHiScore
    // 
    // let whenCourseComplete shipsThrough shs gameTime =
    //     failwith ""
    // 
    // NewAirBattleScreen
    //     StrongEnemy
    //     {Score=10300u ; HiScore=15000u}
    //     3u
    //     whenGameOver
    //     whenCourseComplete
    //     gameTime

    // let whenGameOver scoreAndHiScore gameTime =
    //     NewGameOverScreen scoreAndHiScore
    // 
    // let whenCourseComplete shipsThrough shs gameTime =
    //     failwith ""
    // 
    // NewSeaBattleScreen
    //     {Score=10300u ; HiScore=15000u}
    //     3u
    //     whenGameOver
    //     whenCourseComplete
    //     gameTime

let VictoryStory scoreAndHiScore gameTime =

    let whereToAfter scoreAndHiScore gameTime =
        failwith "title screen"

    NewVictoryScreen scoreAndHiScore whereToAfter gameTime

let rec FinalBossStory mapNumber tanksRemaining scoreAndHiScore finalBossTargets gameTime =

    let whereToOnGameOver      = NewGameOverScreen
    let whereToOnVictory       = VictoryStory
    let whereToOnTankDestroyed = TankBattleStory
    
    NewFinalBossScreen
        mapNumber
        scoreAndHiScore
        tanksRemaining
        finalBossTargets
        whereToOnGameOver
        whereToOnVictory
        whereToOnTankDestroyed
        gameTime
    
and TankBattleStory mapNumber tanksRemaining scoreAndHiScore finalBossTargets gameTime =

    // TODO: We can lambda-bind the boss targets into whenCourseComplete because
    //       the tank battle stage doesn't reduce the boss targets(!)
    //       By passing the boss targets into the tank battle it looks like it could
    //       change them.
    // TODO: these screens should return a new tank count

    let whenGameOver scoreAndHiScore gameTime =
        NewGameOverScreen scoreAndHiScore
    
    let whenCourseComplete tanksRemaining scoreAndHiScore gameTime =
        FinalBossStory mapNumber tanksRemaining scoreAndHiScore finalBossTargets gameTime
    
    match LoadTankBattleSequences () with
        | Error _ -> 
            failwith ""
    
        | Ok tankMaps ->
            NewTankBattleScreen
                scoreAndHiScore
                tanksRemaining
                mapNumber
                tankMaps
                whenGameOver
                whenCourseComplete
                gameTime



let NewFortAssaultStoryboard (gameGlobalState:FortAssaultGlobalState) gameTime =

    let finalBossTargets = NewFinalBossAndTankBattleData ()

    FinalBossStory 1 4u {Score=10000u;HiScore=15000u} finalBossTargets gameTime

