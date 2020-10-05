module ScreenPotentialEnterYourName

open Rules
open DrawingFunctions
open ImagesAndFonts
open FontAlignment
open Geometry
open InputEventData
open EnterYourName
open BeachBackgroundRenderer
open ScoreHiScore
open ScoreboardModel
open ResourceFileMetadata
open StaticResourceAccess

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type PotentialEnterYourNameScreenModel =
    {
        Scoreboard      : ScoreAndName list
        ScoreAndHiScore : ScoreAndHiScore
        DataModel       : EnterYourNameModel
        MemoizedText    : string list
        CanEnterBoard   : bool
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderPotentialEnterYourNameScreen render (model:PotentialEnterYourNameScreenModel) gameTime =

    RenderBeachBackground render gameTime
    Paragraph render BlackFontID CentreAlign MiddleAlign 160<epx> 100<epx> 10<epx> model.MemoizedText

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewPotentialEnterYourNameScreen scoreAndHiScore oldScoreboard =

    let model = NewEnterYourNameModel MaxPlayerNameLength

    {
        Scoreboard      = oldScoreboard
        ScoreAndHiScore = scoreAndHiScore
        DataModel       = model
        MemoizedText    = model |> EnterYourNameModelScreenText
        CanEnterBoard   = scoreAndHiScore.Score |> ScoreCanEnterBoard oldScoreboard
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let ModelUpdatedAccordingToInput model input =

    if input.Fire.JustDown then
        Some (model |> EnterYourNameModelWithInputApplied SelectLetter)

    else if input.Left.JustDown then
        Some (model |> EnterYourNameModelWithInputApplied RotateLeft)

    else if input.Right.JustDown then
        Some (model |> EnterYourNameModelWithInputApplied RotateRight)

    else
        None

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let AddingIntoScoreboard (dataModel:EnterYourNameModel) score oldScoreBoard =

    oldScoreBoard 
        |> WithNewScoreIncluded 
                { 
                    NewPlayerName  = dataModel.Name
                    NewPlayerScore = score
                }
    

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextPotentialEnterYourNameScreenState oldState input _gameTime =

    if oldState.CanEnterBoard then

        let newState =

            match ModelUpdatedAccordingToInput oldState.DataModel input with
                | None -> oldState
                | Some model ->
                    let text = model |> EnterYourNameModelScreenText
                    {
                        Scoreboard      = oldState.Scoreboard       // we only latch a new scoreboard at the very end
                        ScoreAndHiScore = oldState.ScoreAndHiScore  // never changes
                        DataModel       = model
                        MemoizedText    = text
                        CanEnterBoard   = oldState.CanEnterBoard
                    }

        if newState.DataModel |> NameEntryComplete then
            {
                newState with 
                    Scoreboard    = oldState.Scoreboard |> AddingIntoScoreboard newState.DataModel newState.ScoreAndHiScore.Score
                    CanEnterBoard = false // just in case the caller keeps calling us!
            }
        else
            newState
        
    else
        oldState  // You didn't make it to Enter Your Name!

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Query functions for Storyboard
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let StayOnPotentialEnterYourNameScreen state =
    state.CanEnterBoard && not (state.DataModel |> NameEntryComplete)


