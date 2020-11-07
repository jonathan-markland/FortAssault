module Storyboard

open Input
open InterruptableVideo

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

and private AllEatenStory levelNumber scoreAndHiScore gameTime =
 
    WithScreenCompleteIntermissionCard
        scoreAndHiScore
        (PacmanStory (levelNumber + 1) scoreAndHiScore)
        gameTime

and private GameOverStory scoreAndHiScore =
 
    NewGameOverScreen scoreAndHiScore 
        |> AsInterruptableVideoThen
                (EnterYourNameStory scoreAndHiScore)
                (WebBrowserKeyCode 90)  // TODO: Fire button constant?

and private PacmanStory (levelNumber:int) (scoreAndHiScore:ScoreAndHiScore) _gameTime =

    NewPacmanScreen
        levelNumber
        AllEatenStory
        GameOverStory
        scoreAndHiScore

and private GameTitleStory gameTime =

    let scoreAndHiScoreForBrandNewGame = 
        { Score=0u ; HiScore = HiScoreFromScoreboard globalScoreboard }
    
    NewGameTitleScreen globalScoreboard
        |> AsInterruptableVideoThen 
                (PacmanStory 0 scoreAndHiScoreForBrandNewGame)
                (WebBrowserKeyCode 90)  // TODO: Fire button constant?


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewPacmanStoryboard _ gameTime =

    globalScoreboard <- InitialScoreboard [ "Damon" ; "Jarvis" ; "Noel" ; "James" ] 2500u 2500u

    GameTitleStory gameTime
    
