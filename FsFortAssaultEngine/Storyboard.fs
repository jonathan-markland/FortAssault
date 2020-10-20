module Storyboard

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
open TankMapFileLoader
open FortAssaultGlobalState
open ScoreboardModel
open ScreenHandler
open InputEventData

#if SHORT_PLAYTHROUGH
open ResourceIDs
open ImagesAndFonts
open DrawingFunctions
open Geometry
#endif

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
(*
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
*)



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
(*
let RenderFortAssaultStoryboard render gameState gameTime =
    
    let (struct (storyboard , _gameGlobals)) = gameState

    match storyboard with 

        | GameTitleChapter model ->
            RenderGameTitleScreen render model gameTime

        | InitialMapChapter model ->
            RenderInitialMapScreen render model gameTime

        | SecretPassageChapter model ->
            RenderSecretPassageScreen render model gameTime

        | MapPostPassageChapter model ->
            RenderMapPostPassageScreen render model gameTime

        | AirBattleChapter model ->
            RenderAirBattleScreen render model gameTime

        | SeaBattleChapter model ->
            RenderSeaBattleScreen render model gameTime

        | MapBeforeBeachLandingChapter model ->
            RenderMapBeforeBeachLandingScreen render model gameTime

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

    #if SHORT_PLAYTHROUGH
    Text render RedFontID CentreAlign MiddleAlign 160<epx> 10<epx> "WARNING  SHORT PLAY VERSION"
    #endif
    *)

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

(*
let NextStoryboardState staticGameResources gameState keyStateGetter gameTime frameElapsedTime =

    

    let (struct (storyboard , gameGlobals)) = ModelFrom gameState

    /// Where the existing GameGlobalState has NOT changed this frame.
    let inline Advance storyboard =
        let nextModel = 
            struct ((storyboard:Storyboard), gameGlobals)
        gameState |> WithUpdatedModel nextModel

    /// Where the GameGlobalState has been replaced this frame.
    let inline AdvanceAndReplaceGlobals (gameGlobals , storyboard) =
        let nextModel = 
            struct ((storyboard:Storyboard), (gameGlobals:FortAssaultGlobalState))
        gameState |> WithUpdatedModel nextModel

    /// Wrap a 'GET READY' intermission around the transition to a new state.
    let withIntermission desiredNextChapter =
        IntermissionChapter
            (NewIntermissionScreenState gameTime desiredNextChapter)

    match storyboard with

        // Rule:  Battle-cases should check game-over status, not interim screens.
        // Rule:  Enemy strength reduction from using the secret passage only affects air battle.

        | GameTitleChapter model ->  // TODO: Am I calling this "state" or "model" throughout this program?
            Advance (
                let model = NextGameTitleScreenState model keyStateGetter gameTime
                if model |> StayOnTitleScreen then
                    GameTitleChapter model
                else
                    InitialMapChapter (
                        NewInitialMapScreen NumShipsAtInitialEngagement { Score=0u ; HiScore=model.HiScore })
            )
                        
        | InitialMapChapter model ->
            Advance (
                let model = NextInitialMapScreenState model keyStateGetter gameTime
                
                match model |> InitialMapTransition with
                    | StayOnInitialMapScreen ->
                        InitialMapChapter model

                    | FromInitialMapGoToSecretPassage ->
                        withIntermission
                            (fun gameTime ->
                                SecretPassageChapter 
                                    (NewSecretPassageScreen model.ScoreAndHiScore model.NumShips gameTime))

                    | FromInitialMapGoToSeaBattle ->
                        withIntermission
                            (fun gameTime ->
                                AirBattleChapter 
                                    (NewAirBattleScreen StrongEnemy model.ScoreAndHiScore model.NumShips gameTime))
            )

        | SecretPassageChapter model ->
            Advance (
                let model = NextSecretPassageScreenState model keyStateGetter gameTime

                match model |> SecretPassageTransition with
                    | StayOnSecretPassageScreen -> 
                        SecretPassageChapter model
                    
                    | FromSecretPassageGoToNextScreen ->
                        withIntermission
                            (fun gameTime ->
                                MapPostPassageChapter 
                                    (NewMapPostPassageScreen model.FleetStats.Score model.FleetStats.ShipsSuccess))
                    
                    | SecretPassageGameOver ->
                        GameOverChapter (NewGameOverScreen model.FleetStats.Score)
            )

        | MapPostPassageChapter model ->
            Advance (
                let model = NextMapPostPassageScreenState model keyStateGetter gameTime
                if model |> StayOnMapPostPassage then
                    MapPostPassageChapter model
                else
                    AirBattleChapter 
                        (NewAirBattleScreen WeakerEnemy model.ScoreAndHiScore model.ShipsThrough gameTime)
            )

        | AirBattleChapter model ->
            Advance (
                let model = NextAirBattleScreenState model keyStateGetter gameTime frameElapsedTime

                match model |> AirBattleTransition with

                    | StayOnAirBattleScreen -> 
                        AirBattleChapter model

                    | GoToScreenAfterAirBattle ->
                        SeaBattleChapter 
                            (NewSeaBattleScreen model.ScoreAndHiScore model.ShipsRemaining gameTime)

                    | AirBattleGameOver ->
                        GameOverChapter 
                            (NewGameOverScreen model.ScoreAndHiScore)
            )

        | SeaBattleChapter model ->
            Advance (
                let model = NextSeaBattleScreenState model keyStateGetter gameTime frameElapsedTime

                match model |> SeaBattleTransition with
                    
                    | StayOnSeaBattleScreen ->
                        SeaBattleChapter model

                    | GoToScreenAfterSeaBattle ->
                        MapBeforeBeachLandingChapter 
                            (NewMapBeforeBeachLandingScreen model.ScoreAndHiScore model.ShipsRemaining)

                    | SeaBattleGameOver ->
                        GameOverChapter 
                            (NewGameOverScreen model.ScoreAndHiScore)
            )

        | MapBeforeBeachLandingChapter model ->
            Advance (
                let model = NextMapBeforeBeachLandingScreenState model keyStateGetter gameTime

                if model |> StayOnMapBeforeBeachLanding then
                    MapBeforeBeachLandingChapter model

                else
                    withIntermission
                        (fun gameTime ->
                            TankBattleChapter 
                                (NewTankBattleScreen 
                                    model.ScoreAndHiScore
                                    (model.ShipsThrough |> ToTankCountFromShipCount)
                                    (NewFinalBossAndTankBattleData ())
                                    staticGameResources.TankMapsList 
                                    gameTime))
            )

        | TankBattleChapter model ->
            Advance (
                let model = NextTankBattleScreenState model keyStateGetter gameTime frameElapsedTime

                match model |> TankBattleTransition with

                    | StayOnTankBattleScreen ->
                        TankBattleChapter model

                    | TankCompletedCourseSuccessfully ->
                        withIntermission
                            (fun gameTime ->
                                FinalBossChapter 
                                    (NewFinalBossScreen 
                                        model.ScoreAndHiScore
                                        model.TanksRemaining
                                        model.Constants.FinalBossAndTankBattleData
                                        gameTime))

                    | TankBattleGameOver ->
                        GameOverChapter 
                            (NewGameOverScreen model.ScoreAndHiScore)

                    | RestartTankBattle ->
                        withIntermission
                            (fun gameTime ->
                                TankBattleChapter 
                                    (NewTankBattleScreen 
                                        model.ScoreAndHiScore
                                        model.TanksRemaining
                                        model.Constants.FinalBossAndTankBattleData
                                        staticGameResources.TankMapsList
                                        gameTime))
            )

        | FinalBossChapter model ->
            Advance (
                let model = NextFinalBossScreenState model keyStateGetter gameTime frameElapsedTime

                match model |> FinalBossTransition with

                    | StayOnFinalBossScreen ->
                        FinalBossChapter model

                    | VictoryOverFinalBoss ->
                        VictoryChapter 
                            (NewVictoryScreen model.ScoreAndHiScore gameTime)

                    | FinalBossDestroyedTheTank ->
                        TankBattleChapter 
                            (NewTankBattleScreen 
                                model.ScoreAndHiScore
                                model.TanksRemaining
                                model.FinalBossAndTankBattleData
                                staticGameResources.TankMapsList
                                gameTime)

                    | FinalBossGameOver ->
                        GameOverChapter
                            (NewGameOverScreen model.ScoreAndHiScore)
            )

        | IntermissionChapter model ->
            Advance (
                let model = NextIntermissionScreenState model keyStateGetter gameTime

                if model |> StayOnIntermission then
                    IntermissionChapter model
                else
                    model.NextChapterConstructor gameTime
            )

        | GameOverChapter model ->
            Advance (
                let model = NextGameOverScreenState model keyStateGetter gameTime
                if model |> StayOnGameOverScreen then
                    GameOverChapter model
                else
                    PotentialEnterYourNameChapter 
                        (NewPotentialEnterYourNameScreen model.ScoreAndHiScore gameGlobals.GameScoreBoard)
            )
                
        | VictoryChapter model ->
            Advance (
                let model = NextVictoryScreenState model keyStateGetter gameTime

                if model |> StayOnVictoryScreen then
                    VictoryChapter model
                else
                    PotentialEnterYourNameChapter 
                        (NewPotentialEnterYourNameScreen model.ScoreAndHiScore gameGlobals.GameScoreBoard)
            )

        | PotentialEnterYourNameChapter model ->
            AdvanceAndReplaceGlobals (
                let model = NextPotentialEnterYourNameScreenState model keyStateGetter gameTime

                if model |> StayOnPotentialEnterYourNameScreen then
                    let gameGlobals = { gameGlobals with GameScoreBoard = model.Scoreboard }
                    gameGlobals , PotentialEnterYourNameChapter model

                else
                    let gameGlobals = { gameGlobals with GameScoreBoard = model.Scoreboard }
                    gameGlobals , GameTitleChapter (NewGameTitleScreen model.ScoreAndHiScore.HiScore gameGlobals gameTime)
            )
             
        | MechanicsTestPageChapter model ->
            Advance (
                MechanicsTestPageChapter 
                    (NextMechanicsTestPageScreenState model keyStateGetter gameTime))

            *)


