module Storyboard

open Rules
open InterruptableVideo

open Keys

open ScreenGameTitle
open ScreenPacman
open ScreenGameOver
open ScreenPotentialEnterYourName
open PacmanGetReadyOverlay

open ScoreboardModel

open FreezeFrame
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

and private GameOverStory scoreAndHiScore gameTime =
 
    NewGameOverScreen scoreAndHiScore 
        |> WithDrawingOnlyFor GameOverPauseTime gameTime (EnterYourNameStory scoreAndHiScore)

and private PacmanStory (levelNumber:int) betweenScreenStatus _gameTime =

    let newGame _gameTime =
        NewPacmanScreen
            levelNumber
            AllEatenStory
            GameOverStory
            betweenScreenStatus

    let messageOverlay = NewPacmanGetReadyOverlay ()

    FreezeForGetReady newGame messageOverlay GetReadyCardTime _gameTime

and private GameTitleStory gameTime =

    let firstLevelForBrandNewGame =
        0

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
//  TODO: Sort out this sketching:
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

open DrawingFunctions
open DrawingShapes
open ResourceIDs
open BlankScreen
open SlideTransitions
open Mechanics


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewPacmanStoryboard _ gameTime =

    globalScoreboard <- InitialScoreboard [ "DAMON" ; "JARVIS" ; "NOELLE" ; "PAT" ] 1000u 2000u
    
    let NewBlankScreen = NewBlankScreen ScreenWidthInt ScreenHeightInt

    let magentaScreen = NewBlankScreen (SolidColour 0xFF00FFu)
    let yellowScreen  = NewBlankScreen (SolidColour 0xFFFF30u)
    let redScreen     = NewBlankScreen (SolidColour 0xFF0000u)

    NewSlideTransition 
        magentaScreen 
        (GameTitleStory gameTime) ComingFromLeft
        ScreenWidthInt ScreenHeightInt
        SpeedingUpMotion 8.0F<seconds> gameTime 

