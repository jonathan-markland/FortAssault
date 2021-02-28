module Storyboard

open Rules
open SustainModeUntil

open Keys

open ScreenGameTitle
open ScreenGamePlay
open ScreenGameOver
open ScreenPotentialEnterYourName

open ScoreboardModel

open FreezeFrame
open ScreenIntermissions
open ScoreHiScore


open ResourceIDs
open SlideTransitions
open Mechanics
open Time
open Sounds
open GameStateManagement
open StaticResourceAccess
open ResourceIDs


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let mutable private globalScoreboard : ScoreAndName list = []

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

(*let rec private EnterYourNameStory scoreAndHiScore gameTime =

    // You can just direct here, and it will sort out whether to show or not.

    let afterEntry updatedScoreboard gameTime =
        globalScoreboard <- updatedScoreboard
        GameTitleStory gameTime
    
    NewPotentialEnterYourNameScreen 
        scoreAndHiScore
        globalScoreboard
        afterEntry
        gameTime

and private AllEatenStory levelNumber betweenScreenStatus gameTime =
 
    WithScreenCompleteIntermissionCard
        betweenScreenStatus
        (PacmanStory (levelNumber + 1) betweenScreenStatus)
        gameTime

and private GameOverStory scoreAndHiScore gameTime =
 
    let slideInEnterYourName outgoingGameState gameTime = 
        NewSlideTransition 
            outgoingGameState
            (EnterYourNameStory scoreAndHiScore gameTime) ComingFromLeft
            ScreenWidthInt ScreenHeightInt
            SpeedingUpMotion 1.0F<seconds> gameTime 

    NewGameOverScreen scoreAndHiScore 
        |> WithDrawingOnlyFor GameOverPauseTime gameTime slideInEnterYourName
        |> WithOneShotSound [PlaySoundEffect (SoundFromID GameOverSoundID)]

and private PacmanStory (levelNumber:int) betweenScreenStatus (gameTime:float32<seconds>) =

    let newGame _gameTime =
        NewPacmanScreen
            levelNumber
            AllEatenStory
            GameOverStory
            betweenScreenStatus

    let messageOverlay = NewPacmanGetReadyOverlay ()

    FreezeForGetReady newGame messageOverlay GetReadyCardTime gameTime
    
and private *)
let rec GameTitleStory gameTime =

    let betweenScreenStatus = 
        {
            ScoreAndHiScore = { Score=0u ; HiScore = HiScoreFromScoreboard globalScoreboard }
            Lives = InitialLives
        }
    
    let temp shs gt =
        failwith ""

    NewGameTitleScreen globalScoreboard
        |> AsInterruptableVideoThen 
                (NewMissionIIScreen temp betweenScreenStatus)
                KeyFire



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewMissionIIStoryboard _ gameTime =

    globalScoreboard <- InitialScoreboard [ "BARNEY" ; "MARTIN" ; "HELEN" ; "JULIA" ] 2500u 2500u
    
    GameTitleStory gameTime
