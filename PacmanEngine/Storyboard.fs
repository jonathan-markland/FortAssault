﻿module Storyboard

open ScreenGameTitle
open ScreenPacman
open ScreenGameOver
open ScreenPotentialEnterYourName

open ScoreboardModel

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

and private PacmanStory scoreAndHiScore _gameTime =

    NewPacmanScreen
        GameOverStory
        scoreAndHiScore

and private GameTitleStory gameTime =

    let highestScoreInInitialBoard = 
        HiScoreFromScoreboard globalScoreboard
    
    NewGameTitleScreen 
        globalScoreboard
        (PacmanStory {Score=0u ; HiScore=highestScoreInInitialBoard})

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewPacmanStoryboard _ gameTime =

    globalScoreboard <- InitialScoreboard [ "Damon" ; "Jarvis" ; "Noel" ; "James" ] 10000u 5000u

    GameTitleStory gameTime
    