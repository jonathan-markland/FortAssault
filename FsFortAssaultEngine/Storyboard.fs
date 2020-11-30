module Storyboard

open ScreenGameTitle
open ScreenInitialMap
open ScreenSecretPassage
open ScreenMapPostPassage
open ScreenAirBattle
open ScreenSeaBattle
open ScreenMapBeforeBeachLanding
open ScreenTankBattle
open ScreenFinalBoss
open ScreenGameOver
open ScreenVictory
open ScreenPotentialEnterYourName
open MechanicsTestPage

open FinalBossAndTankBattleShared
open Rules
open TankMapFileLoader
open ScoreboardModel

#if SHORT_PLAYTHROUGH
open ResourceIDs
open ImagesAndFonts
open DrawingFunctions
open Geometry
#endif

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let mutable private globalScoreboard : ScoreAndName list = []

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let rec private EnterYourNameStory scoreAndHiScore gameTime =

    // You can just direct here, and it will sort out whether to show or not.

    let afterEntry updatedScoreboard gameTime =
        globalScoreboard <- updatedScoreboard
        GameTitleStory gameTime
    
    NewPotentialEnterYourNameScreen 
        scoreAndHiScore
        globalScoreboard
        afterEntry
        gameTime

and private GameOverStory scoreAndHiScore =
 
    NewGameOverScreen 
        scoreAndHiScore 
        EnterYourNameStory

and private VictoryStory scoreAndHiScore gameTime =

    NewVictoryScreen 
        scoreAndHiScore 
        EnterYourNameStory 
        gameTime

and private FinalBossStory mapNumber tanksRemaining scoreAndHiScore finalBossTargets gameTime =

    NewFinalBossScreen
        mapNumber
        scoreAndHiScore
        tanksRemaining
        finalBossTargets
        GameOverStory
        VictoryStory
        TankBattleStory
        gameTime
    
and private TankBattleStory mapNumber tanksRemaining scoreAndHiScore finalBossTargets gameTime =

    let whenCourseComplete tanksRemaining scoreAndHiScore gameTime =
        FinalBossStory mapNumber tanksRemaining scoreAndHiScore finalBossTargets gameTime
    
    match LoadTankBattleSequences () with
        | Error _ -> 
            failwith "Failed to parse the tank battle level definitions."  // Should never happen as they're not in external files ever since the browser version.
    
        | Ok tankMaps ->
            NewTankBattleScreen
                scoreAndHiScore
                tanksRemaining
                mapNumber
                tankMaps
                GameOverStory
                whenCourseComplete
                gameTime

and private MapBeforeBeachLandingStory shipsThrough scoreAndHiScore gameTime =

    let mapNumber = 0

    let finalBossTargets =
        NewFinalBossAndTankBattleData ()

    let afterLanding gameTime =
        TankBattleStory mapNumber (shipsThrough |> ToTankCountFromShipCount) scoreAndHiScore finalBossTargets gameTime
    
    NewMapBeforeBeachLandingScreen 
        scoreAndHiScore
        shipsThrough
        afterLanding

and private SeaBattleStory shipsRemaining scoreAndHiScore gameTime = 

    NewSeaBattleScreen
        scoreAndHiScore
        shipsRemaining
        GameOverStory
        MapBeforeBeachLandingStory
        gameTime

and private AirBattleStory enemyStrength shipsRemaining scoreAndHiScore gameTime =

    NewAirBattleScreen
        enemyStrength
        scoreAndHiScore
        shipsRemaining
        GameOverStory 
        SeaBattleStory
        gameTime

and private MapPostPassageStory shipsThrough scoreAndHiScore gameTime =

    let afterEngagement gameTime =
        AirBattleStory WeakerEnemy shipsThrough scoreAndHiScore gameTime
    
    NewMapPostPassageScreen
        scoreAndHiScore
        shipsThrough
        afterEngagement

and private SecretPassageScreenStory scoreAndHiScore gameTime =

    NewSecretPassageScreen 
        scoreAndHiScore
        NumShipsAtInitialEngagement
        GameOverStory 
        MapPostPassageStory
        gameTime

and private InitialMapStory scoreAndHiScore =

    let secretPassage gameTime =
        SecretPassageScreenStory scoreAndHiScore gameTime

    let engageEnemy gameTime =
        AirBattleStory StrongEnemy NumShipsAtInitialEngagement scoreAndHiScore gameTime 

    NewInitialMapScreen
        secretPassage
        engageEnemy
        scoreAndHiScore

and private GameTitleStory gameTime =

    let highestScoreInInitialBoard = 
        HiScoreFromScoreboard globalScoreboard
    
    NewGameTitleScreen 
        highestScoreInInitialBoard
        globalScoreboard
        InitialMapStory
        gameTime





let NewFortAssaultStoryboard _ gameTime =

    globalScoreboard <- InitialScoreboard [ "Bob" ; "Scott" ; "Lara" ; "J" ] 10000u 5000u

    GameTitleStory gameTime
    
