module ScreenPotentialEnterYourName

open DrawingFunctions
open ResourceIDs
open Geometry
open EnterYourName
open ScoreHiScore
open ScoreboardModel
open ImagesAndFonts
open GameStateManagement
open Time
open StaticResourceAccess
open Rules
open Input
open Directions
open Keys
open Sounds
open FreezeFrame

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private PotentialEnterYourNameScreenModel =
    {
        Scoreboard         : ScoreAndName list
        ScoreAndHiScore    : ScoreAndHiScore
        EnterYourNameModel : EnterYourNameModel
        MemoizedText       : string list
        WhereToAfterCtor   : ScoreAndName list -> GameTime -> ErasedGameState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderPotentialEnterYourNameScreen render (model:PotentialEnterYourNameScreenModel) gameTime =

    let backgroundImage = Background2ImageID |> ImageFromID
    Image1to1 render 0<epx> 0<epx> backgroundImage
    Paragraph render MissionIIFontID CentreAlign MiddleAlign 160<epx> 100<epx> 10<epx> model.MemoizedText

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private ModelUpdatedAccordingToInput model keyStateGetter =

    let fire  = keyStateGetter KeyFire
    let left  = keyStateGetter KeyLeft
    let right = keyStateGetter KeyRight

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
                    |> WithOneShotSound [PlaySoundEffect (SoundFromID BonusSoundID)]
    
            else
                gameState 
                    |> WithUpdatedModelAndSounds
                        {
                            Scoreboard         = model.Scoreboard       // we only latch a new scoreboard at the very end
                            ScoreAndHiScore    = model.ScoreAndHiScore  // never changes
                            EnterYourNameModel = enterYourNameModel
                            MemoizedText       = enterYourNameModel |> EnterYourNameModelScreenText
                            WhereToAfterCtor   = model.WhereToAfterCtor
                        }
                        [PlaySoundEffect (SoundFromID Footstep1SoundID)]



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


