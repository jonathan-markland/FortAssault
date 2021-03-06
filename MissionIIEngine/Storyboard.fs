﻿module Storyboard

open Mechanics
open Time
open Keys
open SustainModeUntil
open FreezeFrame
open ScoreHiScore
open SlideTransitions

open Rules
open ScreenGameTitle
open ScreenGamePlay
open ScreenGameOver
open ScreenPotentialEnterYourName
open ScoreboardModel
open GamePlayScreenConstants


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

and private GameOverStory scoreAndHiScore gameTime =
 
    let slideInEnterYourName outgoingGameState gameTime = 
        NewSlideTransition 
            outgoingGameState
            (EnterYourNameStory scoreAndHiScore gameTime) ComingFromLeft
            ScreenWidthInt ScreenHeightInt
            SpeedingUpMotion 1.0<seconds> gameTime 

    NewGameOverScreen scoreAndHiScore gameTime
        |> WithDrawingOnlyFor GameOverPauseTime gameTime slideInEnterYourName  // TODO: Can we remove FreezeFrame dependency?  Possibly want to erase FreezeFrame

and private MissionIIStory betweenScreenStatus (gameTime:GameTime) =

    NewMissionIIScreen
        GameOverStory
        betweenScreenStatus
        gameTime
    
and private GameTitleStory gameTime =

    let betweenScreenStatus = 
        {
            ScoreAndHiScore = { Score=0u ; HiScore = HiScoreFromScoreboard globalScoreboard }
            Lives = InitialLives
        }
    
    NewGameTitleScreen globalScoreboard gameTime // TODO: wrap rolling to the hi-score screen in the interruptable video.
        |> AsInterruptableVideoThen 
                (MissionIIStory betweenScreenStatus)
                KeyFire


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewMissionIIStoryboard _ gameTime =

    globalScoreboard <- InitialScoreboard [ "BARNEY" ; "MARTIN" ; "HELEN" ; "JULIA" ] 2500u 2500u
    
    GameTitleStory gameTime
    