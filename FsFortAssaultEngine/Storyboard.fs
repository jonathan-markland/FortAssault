﻿module Storyboard

open ScoreHiScore
open ScreenGameTitle
open ScreenInitialMap
open ScreenSecretPassage
open ScreenMapPostPassage
open ScreenAirBattle
open ScreenSeaBattle
open ScreenMapBeforeBeachLanding
open ScreenTankBattle
open ScreenFinalBoss
open ScreenIntermission
open ScreenGameOver
open ScreenVictory
open ScreenPotentialEnterYourName
open MechanicsTestPage
open FinalBossAndTankBattleShared
open Rules
open StoryboardChapterChange
open TankMapFileLoader
open GameGlobalState

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type Storyboard =

    // In rough order of gameplay:

    | GameTitleChapter              of GameTitleScreenModel
    | InitialMapChapter             of InitialMapScreenModel
    | SecretPassageChapter          of SecretPassageScreenModel
    | MapPostPassageChapter         of MapPostPassageScreenModel
    | AirBattleChapter              of AirBattleScreenModel
    | SeaBattleChapter              of SeaBattleScreenModel
    | MapBeforeBeachLandingChapter  of MapBeforeBeachLandingScreenModel
    | TankBattleChapter             of TankBattleScreenModel
    | FinalBossChapter              of FinalBossScreenModel
    | GameOverChapter               of GameOverScreenModel
    | IntermissionChapter           of IntermissionScreenModel<Storyboard>
    | VictoryChapter                of VictoryScreenModel
    | PotentialEnterYourNameChapter of PotentialEnterYourNameScreenModel

    // Test page:

    | MechanicsTestPageChapter      of MechanicsTestPageScreenModel

type GameResources =
    {
        TankMapsList : TankBattleMapMatrix list  // TODO: We could remove this now the resources are integrated.
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderStoryboard render gameState gameTime =
    
    let (struct (storyboard , _gameGlobals)) = gameState

    match storyboard with 

        | GameTitleChapter model ->
            RenderGameTitleScreen render model gameTime

        | InitialMapChapter model ->
            RenderInitialMapScreen render model

        | SecretPassageChapter model ->
            RenderSecretPassageScreen render gameTime model

        | MapPostPassageChapter model ->
            RenderMapPostPassageScreen render model

        | AirBattleChapter model ->
            RenderAirBattleScreen render model gameTime

        | SeaBattleChapter model ->
            RenderSeaBattleScreen render model gameTime

        | MapBeforeBeachLandingChapter model ->
            RenderMapBeforeBeachLandingScreen render model

        | TankBattleChapter model ->
            RenderTankBattleScreen render model gameTime

        | FinalBossChapter model ->
            RenderFinalBossScreen render model gameTime

        | GameOverChapter model ->
            RenderGameOverScreen render model gameTime

        | IntermissionChapter model ->
            RenderIntermissionScreen render model gameTime

        | VictoryChapter model ->
            RenderVictoryScreen render model gameTime

        | PotentialEnterYourNameChapter model ->
            RenderPotentialEnterYourNameScreen render model gameTime

        | MechanicsTestPageChapter model ->
            RenderMechanicsTestPageScreen render model gameTime

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//   Shortcuts to levels for development purposes
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
            
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
    | SkipToMechanicsTestPage

/// Provide access to developer shortcuts to various screens.
/// Use RunGameNormally to start the game normally.
let Shortcut gameResources gameTime mode =

    match mode with

        | RunGameNormally ->
            // -- THIS CASE EXECUTES FOR THE RELEASE VERSION --
            GameTitleChapter(NewGameTitleScreen DefaultHiScore (InitialGameGlobals ()) gameTime)

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
            let screen = NewPotentialEnterYourNameScreen {Score=25000u ; HiScore=25000u}  // ie: you got the new hi score compared to InitialGameGlobals()
            PotentialEnterYourNameChapter screen

        | SkipToGameOverScreen ->
            // Shortcut to game over screen
            let screen = NewGameOverScreen {Score=25000u ; HiScore=25000u}  // ie: you got the new hi score compared to InitialGameGlobals()
            GameOverChapter screen

        | SkipToMechanicsTestPage ->
            // Shortcut to the mechanics test page
            let screen = NewMechanicsTestPageScreen gameTime
            MechanicsTestPageChapter screen
         

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//   Creation of new storyboard
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Called only once when the game boots
let NewStoryboard gameResources gameTime =

    Shortcut gameResources gameTime RunGameNormally

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextStoryboardState staticGameResources gameState input gameTime frameElapsedTime =

    let (struct (storyboard , gameGlobals)) = gameState

    /// Where the existing GameGlobalState has NOT changed this frame.
    let inline Advance storyboard =
        struct ((storyboard:Storyboard), gameGlobals)

    /// Where the GameGlobalState has been replaced this frame.
    let inline AdvanceAndReplaceGlobals (gameGlobals , storyboard) =
        struct ((storyboard:Storyboard), (gameGlobals:GameGlobalState))

    /// Wrap a 'GET READY' intermission around the transition to a new state.
    let withIntermission desiredNextChapter =
        IntermissionChapter
            (NewIntermissionScreenState gameTime desiredNextChapter)
    
    match storyboard with

        // Rule:  Battle-cases should check game-over status, not interim screens.
        // Rule:  Enemy strength reduction from using the secret passage only affects air battle.

        | GameTitleChapter model ->
            Advance
                (match NextGameTitleScreenState model input gameTime with

                    | StayOnThisChapter1 newModel -> 
                        GameTitleChapter newModel

                    | GoToNextChapter1 newModel ->
                        InitialMapChapter (
                            NewInitialMapScreen NumShipsAtInitialEngagement { Score=0u ; HiScore=newModel.HiScore }))
                        
        | InitialMapChapter model ->
            Advance
                (match NextInitialMapScreenState model input gameTime with

                    | StayOnInitialMap newModel ->
                         InitialMapChapter newModel

                    | GoToSecretPassage newModel ->
                        withIntermission
                            (fun gameTime ->
                                let secretPassageModel = 
                                    NewSecretPassageScreen newModel.ScoreAndHiScore newModel.NumShips gameTime
                                SecretPassageChapter secretPassageModel)

                    | GoToBattleAtSea newModel ->
                        withIntermission
                            (fun gameTime ->
                                let airBattleModel = 
                                    NewAirBattleScreen StrongEnemy newModel.ScoreAndHiScore newModel.NumShips gameTime
                                AirBattleChapter airBattleModel))

        | SecretPassageChapter model ->
            Advance
                (match NextSecretPassageScreenState model input gameTime with

                    | StayOnThisChapter2 newModel -> 
                        SecretPassageChapter newModel

                    | GoToNextChapter2 newModel ->
                        withIntermission
                            (fun gameTime ->
                                let postPassageModel = 
                                    NewMapPostPassageScreen newModel.FleetStats.Score newModel.FleetStats.ShipsSuccess
                                MapPostPassageChapter postPassageModel)

                    | GameOver2 newModel ->
                        let gameOverModel = 
                            NewGameOverScreen newModel.FleetStats.Score
                        GameOverChapter gameOverModel)

        | MapPostPassageChapter model ->
            Advance
                (match NextMapPostPassageScreenState model input gameTime with

                    | StayOnThisChapter1 newModel ->
                        MapPostPassageChapter newModel

                    | GoToNextChapter1 newModel ->
                        let airBattleModel =
                            NewAirBattleScreen WeakerEnemy newModel.ScoreAndHiScore model.ShipsThrough gameTime
                        AirBattleChapter airBattleModel)

        | AirBattleChapter model ->
            Advance
                (match NextAirBattleScreenState model input gameTime frameElapsedTime with

                    | StayOnThisChapter2 newModel -> 
                        AirBattleChapter newModel

                    | GoToNextChapter2 newModel ->
                        let seaBattleModel = 
                            NewSeaBattleScreen newModel.ScoreAndHiScore newModel.ShipsRemaining gameTime
                        SeaBattleChapter seaBattleModel

                    | GameOver2 newModel ->
                        let gameOverModel = 
                            NewGameOverScreen newModel.ScoreAndHiScore
                        GameOverChapter gameOverModel)

        | SeaBattleChapter model ->
            Advance
                (match NextSeaBattleScreenState model input gameTime frameElapsedTime with

                    | StayOnThisChapter2 newModel ->
                        SeaBattleChapter newModel

                    | GoToNextChapter2 newModel ->
                        let beachLandingModel = 
                            NewMapBeforeBeachLandingScreen newModel.ScoreAndHiScore newModel.ShipsRemaining
                        MapBeforeBeachLandingChapter beachLandingModel

                    | GameOver2 newModel ->
                        let gameOverModel = 
                            NewGameOverScreen newModel.ScoreAndHiScore
                        GameOverChapter gameOverModel)

        | MapBeforeBeachLandingChapter model ->
            Advance
                (match NextMapBeforeBeachLandingScreenState model input gameTime with

                    | StayOnThisChapter1 newModel ->
                        MapBeforeBeachLandingChapter newModel

                    | GoToNextChapter1 newModel ->
                        withIntermission
                            (fun gameTime ->
                                let finalBossAndTankBattleData = 
                                    NewFinalBossAndTankBattleData ()
                                let tankBattleModel = 
                                    NewTankBattleScreen 
                                        newModel.ScoreAndHiScore
                                        (newModel.ShipsThrough |> ToTankCountFromShipCount) 
                                        finalBossAndTankBattleData 
                                        staticGameResources.TankMapsList 
                                        gameTime
                                TankBattleChapter tankBattleModel))

        | TankBattleChapter model ->
            Advance
                (match NextTankBattleScreenState model input gameTime frameElapsedTime with

                    | StayOnTankBattleScreen newModel ->
                        TankBattleChapter newModel

                    | TankCompletedCourseSuccessfully newModel ->
                        withIntermission
                            (fun gameTime ->
                                let finalBossModel = 
                                    NewFinalBossScreen 
                                        newModel.ScoreAndHiScore
                                        newModel.TanksRemaining
                                        newModel.Constants.FinalBossAndTankBattleData
                                        gameTime
                                FinalBossChapter finalBossModel)

                    | GameOverOnTankBattleScreen newModel ->
                        let gameOverModel = NewGameOverScreen newModel.ScoreAndHiScore
                        GameOverChapter gameOverModel

                    | RestartTankBattle newModel ->
                        withIntermission
                            (fun gameTime ->
                                let tankBattleModel = 
                                    NewTankBattleScreen 
                                        newModel.ScoreAndHiScore
                                        newModel.TanksRemaining
                                        newModel.Constants.FinalBossAndTankBattleData
                                        staticGameResources.TankMapsList
                                        gameTime
                                TankBattleChapter tankBattleModel))

        | FinalBossChapter model ->
            Advance
                (match NextFinalBossScreenState model input gameTime frameElapsedTime with

                    | StayOnFinalBossScreen newModel ->
                        FinalBossChapter newModel

                    | Victory newModel ->
                        let victoryModel = NewVictoryScreen newModel.ScoreAndHiScore gameTime
                        VictoryChapter victoryModel

                    | FinalBossDestroyedTheTank newModel ->
                        let tankBattleModel = 
                            NewTankBattleScreen 
                                newModel.ScoreAndHiScore
                                newModel.TanksRemaining
                                newModel.FinalBossAndTankBattleData
                                staticGameResources.TankMapsList
                                gameTime
                        TankBattleChapter tankBattleModel

                    | GameOverBecauseOfFinalBoss newModel ->
                        let gameOverModel = NewGameOverScreen newModel.ScoreAndHiScore
                        GameOverChapter gameOverModel)

        | IntermissionChapter model ->
            Advance
                (match NextIntermissionScreenState model input gameTime with

                    | StayOnThisChapter1 newModel ->
                        IntermissionChapter newModel

                    | GoToNextChapter1 newModel ->
                        (newModel.NextChapterConstructor gameTime))

        | GameOverChapter model ->
            Advance
                (match NextGameOverScreenState model input gameTime with

                    | StayOnThisChapter1 newModel ->
                        GameOverChapter newModel

                    | GoToNextChapter1 newModel ->
                        PotentialEnterYourNameChapter (NewPotentialEnterYourNameScreen newModel.ScoreAndHiScore))
                
        | VictoryChapter model ->
            Advance
                (match NextVictoryScreenState model input gameTime with

                    | StayOnThisChapter1 newModel ->
                        VictoryChapter newModel

                    | GoToNextChapter1 newModel ->
                        PotentialEnterYourNameChapter (NewPotentialEnterYourNameScreen newModel.ScoreAndHiScore))

        | PotentialEnterYourNameChapter model ->
            AdvanceAndReplaceGlobals
                (match NextPotentialEnterYourNameScreenState gameGlobals.GameScoreBoard model input gameTime with

                    | StayOnThisChapter1(newScoreboard, newModel) ->
                        let gameGlobals = { gameGlobals with GameScoreBoard=newScoreboard }
                        gameGlobals , PotentialEnterYourNameChapter newModel

                    | GoToNextChapter1(newScoreboard, newModel) ->
                        let gameGlobals = { gameGlobals with GameScoreBoard=newScoreboard }
                        gameGlobals , GameTitleChapter (NewGameTitleScreen newModel.ScoreAndHiScore.HiScore gameGlobals gameTime))
             
        | MechanicsTestPageChapter model ->
            Advance
                (MechanicsTestPageChapter (NextMechanicsTestPageScreenState model input gameTime))
