module ScreenPotentialEnterYourName

open DrawingFunctions
open ResourceIDs
open Geometry
open EnterYourName
open ScoreHiScore
open ScoreboardModel
open ImagesAndFonts
open ScreenHandler
open Time
open StaticResourceAccess
open Rules
open Input
open PacmanShared
open TitleScreenShared

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private PotentialEnterYourNameScreenModel =
    {
        Scoreboard         : ScoreAndName list
        ScoreAndHiScore    : ScoreAndHiScore
        EnterYourNameModel : EnterYourNameModel
        MemoizedText       : string list
        PacRightMemo       : TitleScreenPacmanState
        WhereToAfterCtor   : ScoreAndName list -> float32<seconds> -> ErasedGameState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderPotentialEnterYourNameScreen render (model:PotentialEnterYourNameScreenModel) gameTime =

    let backgroundImage = BackgroundImageID |> ImageFromID
    Image1to1 render 0<epx> 0<epx> backgroundImage

    let tilesImage = 
        Level1ImageID |> ImageFromID

    let pacAt = 
        DrawPacMan render tilesImage gameTime

    pacAt  model.PacRightMemo

    Paragraph render GreyFontID CentreAlign MiddleAlign 160<epx> 100<epx> 10<epx> model.MemoizedText

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private ModelUpdatedAccordingToInput model keyStateGetter =

    let fire  = keyStateGetter (WebBrowserKeyCode 90)
    let left  = keyStateGetter (WebBrowserKeyCode 37)
    let right = keyStateGetter (WebBrowserKeyCode 39)

    if fire.JustDown then
        Some (model |> EnterYourNameModelWithInputApplied SelectLetter)
    
    else if left.JustDown then
        Some (model |> EnterYourNameModelWithInputApplied RotateLeft)
    
    else if right.JustDown then
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

    let model = ModelFrom gameState
    
    match ModelUpdatedAccordingToInput model.EnterYourNameModel keyStateGetter with
        
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
                        PacRightMemo       = model.PacRightMemo
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
                PacRightMemo       = TitleScreenPac FacingLeft  50 15
            }

        NewGameState NextPotentialEnterYourNameScreenState RenderPotentialEnterYourNameScreen screenModel

    else

        // The player didn't enter the scoreboard, so we don't actually stay
        // on this screen at all.

        whereToAfter oldScoreboard gameTime


