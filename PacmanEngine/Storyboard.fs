module Storyboard

open Rules
open InterruptableVideo

open Keys

open ScreenGameTitle
open ScreenPacman
open ScreenGameOver
open ScreenPotentialEnterYourName

open ScoreboardModel

open ScreenIntermissions
open ScoreHiScore

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

and private AllEatenStory levelNumber betweenScreenStatus gameTime =
 
    WithScreenCompleteIntermissionCard
        betweenScreenStatus
        (PacmanStory (levelNumber + 1) betweenScreenStatus)
        gameTime

and private GameOverStory scoreAndHiScore =
 
    NewGameOverScreen scoreAndHiScore 
        |> AsInterruptableVideoThen
                (EnterYourNameStory scoreAndHiScore)
                KeyFire

and private PacmanStory (levelNumber:int) betweenScreenStatus _gameTime =

    NewPacmanScreen
        levelNumber
        AllEatenStory
        GameOverStory
        betweenScreenStatus

and private GameTitleStory gameTime =

    let firstLevelForBrandNewGame =
        0 // 125 -- with dead ends

    let betweenScreenStatus = 
        {
            ScoreAndHiScore = { Score=0u ; HiScore = HiScoreFromScoreboard globalScoreboard }
            Lives = InitialLives
        }
    
    NewGameTitleScreen globalScoreboard
        |> AsInterruptableVideoThen 
                (PacmanStory firstLevelForBrandNewGame betweenScreenStatus)
                KeyFire


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewPacmanStoryboard _ gameTime =

    globalScoreboard <- InitialScoreboard [ "Damon" ; "Jarvis" ; "Noel" ; "James" ] 2500u 2500u

    GameTitleStory gameTime
    
