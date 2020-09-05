module ScreenPotentialEnterYourName

open Rules
open DrawingCommandsEx
open ImagesAndFonts
open FontAlignment
open Geometry
open InputEventData
open StoryboardChapterChange
open EnterYourName
open BeachBackgroundRenderer
open ScoreHiScore
open ScoreboardModel
open ResourceFileMetadata
open StaticResourceAccess

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type PotentialEnterYourNameScreenModel =
    {
        ScoreAndHiScore : ScoreAndHiScore
        DataModel       : EnterYourNameModel
        MemoizedText    : string list
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderPotentialEnterYourNameScreen render (model:PotentialEnterYourNameScreenModel) gameTime =

    RenderBeachBackground render gameTime
    Paragraph render BlackFontID CentreAlign MiddleAlign 160<epx> 75<epx> 10<epx> model.MemoizedText

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewPotentialEnterYourNameScreen scoreAndHiScore =

    let model = NewEnterYourNameModel MaxPlayerNameLength

    {
        ScoreAndHiScore = scoreAndHiScore
        DataModel       = model
        MemoizedText    = model |> EnterYourNameModelScreenText
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

let NextPotentialEnterYourNameScreenState scoreBoard oldState input _gameTime =

    let score = oldState.ScoreAndHiScore.Score

    if score |> ScoreCanEnterBoard scoreBoard then  // TODO: Small issue with this design is we re-evaluate this every frame.

        let newState =

            match ModelUpdatedAccordingToInput oldState.DataModel input with
                | None -> oldState
                | Some model ->
                    let text = model |> EnterYourNameModelScreenText
                    {
                        ScoreAndHiScore = oldState.ScoreAndHiScore  // never changes
                        DataModel       = model
                        MemoizedText    = text
                    }


        if newState.DataModel |> NameEntryComplete then
            GoToNextChapter1(scoreBoard |> AddingIntoScoreboard newState.DataModel score, newState)
        else
            StayOnThisChapter1(scoreBoard, newState)
        
    else

        // You didn't make it to Enter Your Name!

        GoToNextChapter1(scoreBoard, oldState)





