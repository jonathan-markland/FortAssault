module ScreenPotentialEnterYourName

open Rules
open DrawingFunctions
open ResourceIDs
open Geometry
open InputEventData
open EnterYourName
open BeachBackgroundRenderer
open ScoreHiScore
open ScoreboardModel
open ImagesAndFonts
open ScreenHandler
open Time

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private PotentialEnterYourNameScreenModel =
    {
        Scoreboard         : ScoreAndName list
        ScoreAndHiScore    : ScoreAndHiScore
        EnterYourNameModel : EnterYourNameModel
        MemoizedText       : string list
        WhereToAfterCtor   : ScoreAndName list -> float32<seconds> -> ErasedGameState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderPotentialEnterYourNameScreen render (model:PotentialEnterYourNameScreenModel) gameTime =

    RenderBeachBackground render gameTime
    Paragraph render BlackFontID CentreAlign MiddleAlign 160<epx> 100<epx> 10<epx> model.MemoizedText

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private ModelUpdatedAccordingToInput model input =

    if input.Fire.JustDown then
        Some (model |> EnterYourNameModelWithInputApplied SelectLetter)

    else if input.Left.JustDown then
        Some (model |> EnterYourNameModelWithInputApplied RotateLeft)

    else if input.Right.JustDown then
        Some (model |> EnterYourNameModelWithInputApplied RotateRight)

    else
        None

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private AddingIntoScoreboard (dataModel:EnterYourNameModel) score oldScoreBoard =

    oldScoreBoard 
        |> WithNewScoreIncluded 
                { 
                    NewPlayerName  = dataModel.Name
                    NewPlayerScore = score
                }
    

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextPotentialEnterYourNameScreenState gameState keyStateGetter gameTime _elapsed =

    // We are only here because the name could enter the board in the first place.

    let input = keyStateGetter |> DecodedInput
    let model = ModelFrom gameState

    match ModelUpdatedAccordingToInput model.EnterYourNameModel input with
        
        | None -> 
            Unchanged gameState

        | Some enterYourNameModel ->
            
            if enterYourNameModel |> NameEntryComplete then
                
                let updatedScoreboard =
                    model.Scoreboard 
                        |> AddingIntoScoreboard enterYourNameModel model.ScoreAndHiScore.Score

                model.WhereToAfterCtor updatedScoreboard gameTime

            else
                gameState |> WithUpdatedModel
                    {
                        Scoreboard         = model.Scoreboard       // we only latch a new scoreboard at the very end
                        ScoreAndHiScore    = model.ScoreAndHiScore  // never changes
                        EnterYourNameModel = enterYourNameModel
                        MemoizedText       = enterYourNameModel |> EnterYourNameModelScreenText
                        WhereToAfterCtor   = model.WhereToAfterCtor
                    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewPotentialEnterYourNameScreen scoreAndHiScore oldScoreboard whereToAfter gameTime =

    if scoreAndHiScore.Score |> ScoreCanEnterBoard oldScoreboard then

        let enterYourNameModel = 
            NewEnterYourNameModel MaxPlayerNameLength

        let screenModel =
            {
                Scoreboard         = oldScoreboard
                ScoreAndHiScore    = scoreAndHiScore
                EnterYourNameModel = enterYourNameModel
                MemoizedText       = enterYourNameModel |> EnterYourNameModelScreenText
                WhereToAfterCtor   = whereToAfter
            }

        NewGameState NextPotentialEnterYourNameScreenState RenderPotentialEnterYourNameScreen screenModel

    else

        // The player didn't enter the scoreboard, so we don't actually stay
        // on this screen at all.

        whereToAfter oldScoreboard gameTime


