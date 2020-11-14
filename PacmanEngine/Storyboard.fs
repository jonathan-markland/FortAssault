module Storyboard

open Input
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

and private AllEatenStory levelNumber scoreAndHiScore gameTime =
 
    WithScreenCompleteIntermissionCard
        scoreAndHiScore
        (PacmanStory (levelNumber + 1) scoreAndHiScore)
        gameTime

and private GameOverStory scoreAndHiScore =
 
    NewGameOverScreen scoreAndHiScore 
        |> AsInterruptableVideoThen
                (EnterYourNameStory scoreAndHiScore)
                KeyFire

and private PacmanStory (levelNumber:int) (scoreAndHiScore:ScoreAndHiScore) _gameTime =

    NewPacmanScreen
        levelNumber
        AllEatenStory
        GameOverStory
        scoreAndHiScore

and private GameTitleStory gameTime =

    let firstLevelForBrandNewGame =
        145 // 125 -- with dead ends

    let scoreAndHiScoreForBrandNewGame = 
        { Score=0u ; HiScore = HiScoreFromScoreboard globalScoreboard }
    
    NewGameTitleScreen globalScoreboard
        |> AsInterruptableVideoThen 
                (PacmanStory firstLevelForBrandNewGame scoreAndHiScoreForBrandNewGame)
                KeyFire


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

open ProbabilityTable // TODO: remove
open GhostMoveTraits

let NewPacmanStoryboard _ gameTime =

    let table = MovementProbabilitiesTable ()

    let dirMaskToIndex mask =
        match mask with
            | 8uy -> 0uy
            | 4uy -> 1uy
            | 2uy -> 2uy
            | 1uy -> 3uy
            | _ -> failwith "Invalid direction bitmask"

    let railsByteThenFacingDirection row =
        let 
            {
                CaseDescription    = _
                Rails              = railsByte
                EntryDirectionMask = entryMask
                Probs              = _
            }
                = row

        (railsByte, entryMask |> dirMaskToIndex)

    let unicodeBoxDrawing = " ╷╶┌╵│└├╴┐─┬┘┤┴┼●"

    let printfAsCode i row =
        let shapeIndex = ((i / 4) + 1)
        printfn 
            "{ PCLeft=%s ; PCUp=%s ; PCRight=%s ; PCDown=%s } // '%s' %d. %d %s"
            (sprintf "%A" row.Probs.PCLeft)
            (sprintf "%A" row.Probs.PCUp)
            (sprintf "%A" row.Probs.PCRight)
            (sprintf "%A" row.Probs.PCDown)
            (unicodeBoxDrawing.[shapeIndex].ToString())
            i
            shapeIndex
            row.CaseDescription

    table 
        |> List.toArray
        |> Array.sortBy railsByteThenFacingDirection
        |> Array.iteri printfAsCode

    globalScoreboard <- InitialScoreboard [ "Damon" ; "Jarvis" ; "Noel" ; "James" ] 2500u 2500u

    GameTitleStory gameTime
    
