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
//   Shortcuts to levels for development purposes
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    (*        
type DevelopmentShortcutMode =
    | RunGameNormally
    | SkipToInitialMap
    | SkipToIntermissionTest
    | SkipToSecretPassage
    | SkipToMapPostPassage
    | SkipToAirBattle
    | SkipToSeaBattle
    | SkipToMapBeforeBeachLanding
    | SkipToTankBattle
    | SkipToFinalBoss
    | SkipToVictoryScreen
    | SkipToEnterYourName
    | SkipToGameOverScreen
    | SkipToMechanicsTestPage*)

(*

/// Provide access to developer shortcuts to various screens.
/// Use RunGameNormally to start the game normally.
let Shortcut gameResources gameTime mode =

    match mode with

        | RunGameNormally ->
            // -- THIS CASE EXECUTES FOR THE RELEASE VERSION --
            match FortAssaultGlobalStateConstructor () with
                | Error msg -> failwith msg
                | Ok gameGlobals ->
                    let highestScoreInInitialBoard = HiScoreFromScoreboard gameGlobals.GameScoreBoard
                    GameTitleChapter(NewGameTitleScreen highestScoreInInitialBoard gameGlobals gameTime)
        // -- NONE OF THE FOLLOWING CASES EXECUTE FOR THE RELEASE VERSION --

        | SkipToInitialMap ->
            // Shortcut to the initial map chapter
            let initialMapModel = NewInitialMapScreen NumShipsAtInitialEngagement {Score=0u ; HiScore=12345u}
            InitialMapChapter initialMapModel

        | SkipToIntermissionTest ->
            // Test intermission then go to secret passage
            let desiredNextChapter = 
                (fun gameTime -> 
                    SecretPassageChapter
                        (NewSecretPassageScreen {Score=0u;HiScore=10000u} 7u gameTime))

            IntermissionChapter
                (NewIntermissionScreenState gameTime desiredNextChapter)

        | SkipToSecretPassage ->
            // Shortcut to secret passage
            let secretPassageModel = NewSecretPassageScreen {Score=0u;HiScore=10000u} 7u gameTime
            SecretPassageChapter secretPassageModel

        | SkipToMapPostPassage ->
            // Shortcut to map (post passage) chapter
            let postPassageModel = NewMapPostPassageScreen {Score=0u;HiScore=10000u} 5u
            MapPostPassageChapter postPassageModel

        | SkipToAirBattle ->
            // Shortcut to air battle screen
            let screen = NewAirBattleScreen StrongEnemy {Score=100u;HiScore=1000u} 4u gameTime
            AirBattleChapter screen

        | SkipToSeaBattle ->
            // Shortcut to sea battle screen
            let screen = NewSeaBattleScreen {Score=100u;HiScore=1000u} 4u gameTime
            SeaBattleChapter screen

        | SkipToMapBeforeBeachLanding ->
            // Shortcut to map before beach landing
            let beachLandingModel = NewMapBeforeBeachLandingScreen {Score=100u;HiScore=1000u} 4u
            MapBeforeBeachLandingChapter beachLandingModel

        | SkipToTankBattle ->
            // Shortcut to tank battle screen
            let finalBossAndTankBattleData = NewFinalBossAndTankBattleData ()
            let screen = NewTankBattleScreen {Score=100u;HiScore=1000u} 8u finalBossAndTankBattleData gameResources.TankMapsList gameTime
            TankBattleChapter screen

        | SkipToFinalBoss ->
            // Shortcut to final boss screen
            let finalBossAndTankBattleData = NewFinalBossAndTankBattleData ()
            let screen = NewFinalBossScreen {Score=100u;HiScore=1000u} 3u finalBossAndTankBattleData gameTime
            FinalBossChapter screen

        | SkipToVictoryScreen ->
            // Shortcut to victory screen
            let screen = NewVictoryScreen {Score=25000u ; HiScore=37000u} gameTime
            VictoryChapter screen

        | SkipToEnterYourName ->
            // Shortcut to Enter your name screen
            match FortAssaultGlobalStateConstructor () with
                | Error msg -> failwith msg
                | Ok globals ->
                    let screen = NewPotentialEnterYourNameScreen {Score=25000u ; HiScore=25000u} globals.GameScoreBoard  // ie: you got the new hi score compared to InitialGameGlobals()
                    PotentialEnterYourNameChapter screen

        | SkipToGameOverScreen ->
            // Shortcut to game over screen
            let screen = NewGameOverScreen {Score=25000u ; HiScore=25000u}  // ie: you got the new hi score compared to InitialGameGlobals()
            GameOverChapter screen

        | SkipToMechanicsTestPage ->
            // Shortcut to the mechanics test page
            let screen = NewMechanicsTestPageScreen gameTime
            MechanicsTestPageChapter screen
  *)       

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
    
