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






let rec GameOverStory scoreAndHiScore =

    let hack = 
        {
            GameScoreBoard = InitialScoreboard [ "Bob" ; "Scott" ; "Lara" ; "J" ] 10000u 5000u
        }

    let afterEntry updatedScoreboard gameTime =
        GameTitleStory { GameScoreBoard = updatedScoreboard } gameTime
        
    let whereAfterGameOver scoreAndHiScore gameTime = 
        NewPotentialEnterYourNameScreen 
            scoreAndHiScore
            hack.GameScoreBoard
            afterEntry
            gameTime

    NewGameOverScreen whereAfterGameOver scoreAndHiScore

and VictoryStory scoreAndHiScore gameTime =

    let whereToAfter scoreAndHiScore gameTime =
        failwith "title screen"

    NewVictoryScreen scoreAndHiScore whereToAfter gameTime

and FinalBossStory mapNumber tanksRemaining scoreAndHiScore finalBossTargets gameTime =

    let whereToOnGameOver      = GameOverStory
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

    let whenGameOver =
        GameOverStory 
    
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

and MapBeforeBeachLandingStory shipsThrough scoreAndHiScore gameTime =

    let mapNumber = 0

    let finalBossTargets =
        NewFinalBossAndTankBattleData ()

    let afterLanding gameTime =
        TankBattleStory mapNumber (shipsThrough |> ToTankCountFromShipCount) scoreAndHiScore finalBossTargets gameTime
    
    NewMapBeforeBeachLandingScreen 
        scoreAndHiScore
        shipsThrough
        afterLanding

and SeaBattleStory shipsRemaining scoreAndHiScore gameTime = 

    let whenGameOver =
        GameOverStory 
    
    let whenCourseComplete =
        MapBeforeBeachLandingStory
    
    NewSeaBattleScreen
        scoreAndHiScore
        shipsRemaining
        whenGameOver
        whenCourseComplete
        gameTime

and AirBattleStory enemyStrength shipsRemaining scoreAndHiScore gameTime =

    let whenGameOver =
        GameOverStory 
    
    let whenCourseComplete =
        SeaBattleStory
    
    NewAirBattleScreen
        enemyStrength
        scoreAndHiScore
        shipsRemaining
        whenGameOver
        whenCourseComplete
        gameTime

and MapPostPassageStory shipsThrough scoreAndHiScore gameTime =

    let afterEngagement gameTime =
        AirBattleStory WeakerEnemy shipsThrough scoreAndHiScore gameTime
    
    NewMapPostPassageScreen
        scoreAndHiScore
        shipsThrough
        afterEngagement

and SecretPassageScreenStory scoreAndHiScore gameTime =

    let whenGameOver =
        GameOverStory 
    
    let whenCourseComplete =
        MapPostPassageStory
    
    NewSecretPassageScreen 
        scoreAndHiScore
        NumShipsAtInitialEngagement
        whenGameOver
        whenCourseComplete
        gameTime

and InitialMapStory scoreAndHiScore =

    let secretPassage gameTime =
        SecretPassageScreenStory scoreAndHiScore gameTime

    let engageEnemy gameTime =
        AirBattleStory StrongEnemy NumShipsAtInitialEngagement scoreAndHiScore gameTime 

    NewInitialMapScreen
        secretPassage
        engageEnemy
        scoreAndHiScore

and GameTitleStory gameGlobalState gameTime =

    let currentScoreboard =
        gameGlobalState.GameScoreBoard

    let highestScoreInInitialBoard = 
        HiScoreFromScoreboard currentScoreboard
    
    NewGameTitleScreen 
        highestScoreInInitialBoard 
        gameGlobalState 
        InitialMapStory
        gameTime





let NewFortAssaultStoryboard (gameGlobalState:FortAssaultGlobalState) gameTime =

    GameTitleStory gameGlobalState gameTime
    
