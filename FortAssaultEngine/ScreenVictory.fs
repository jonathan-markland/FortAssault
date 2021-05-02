module ScreenVictory

open Time
open DrawingFunctions
open ScoreHiScore
open Geometry
open InputEventData
open ResourceIDs
open ImagesAndFonts
open StaticResourceAccess
open GameStateManagement

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private VictoryScreenModel =
    {
        ScoreAndHiScore     : ScoreAndHiScore
        ScoreText           : string
        HiScoreText         : string
        NextAnimStageTime   : GameTime
        AnimationStage      : string list
        WhereToAfterCtor    : ScoreAndHiScore -> GameTime -> ErasedGameState
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//
// Animation sequence
// ------------------
//
// Medal ceremony leading into salute sequence.
//
// - We start the animation sequence immediately that the screen starts.
// - When the sequence expires, all images are shown in the default states.
// - President default state is standing face on, arms by side.
// - Colonel and officers default state is standing face on, arms by side.
// - Medal default state is on the Colonel's uniform.
// - Default states are overridden by letters indicated below:

let private TimePerAnimStage = 1.0<seconds>

//           |v---- P = President holds out hand  | Either of these implies showing the medal in the hands position.
//           | v--- C = Colonel holds out hand    | 
//           | 
//           |  v-- x = Medal transfer phase

let private MedalSequence =
    [
        "----|--x"
        "----|--x"
        "----|P-x"
        "----|PCx"
        "----|-Cx"
        "----|---"
        "----|---"
    ]

//          v---- P = President holds salute
//         v----- 3 = Colonel holds salute
//        v------ 2 = Officer #2 holds salute
//       v------- 1 = Officer #1 holds salute

let private SaluteSequence =
    [
        "---P|---"
        "123P|---"
        "123P|---"
        "123P|---"
        "123-|---"
    ]

let private MickeySaluteSequence =
    [
        "---P|---"
        "-23P|---"
        "-23P|---"
        "-23P|---"
        "-23-|---"
        "1---|---"
        "1--P|---"
        "123P|---"
        "123P|---"
        "123P|---"
        "123-|---"
    ]

let private pauseAfterwardsSequence =
    [
        "----|---"
        "----|---"
        "----|---"
        "----|---"
    ]

let private NormalAnimation = MedalSequence @ SaluteSequence       @ pauseAfterwardsSequence
let private MickeyAnimation = MedalSequence @ MickeySaluteSequence @ pauseAfterwardsSequence

let private AnimationSequence gameTime =
    if (gameTime % 4.0<seconds>) < 1.0<seconds> then MickeyAnimation else NormalAnimation

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderAnimationStage render (currentStage:string) =

    let medalTransferPhase    = (currentStage.[7] = 'x')
    let colonelHoldsOutHand   = (currentStage.[6] = 'C')
    let presidentHoldsOutHand = (currentStage.[5] = 'P')
    let presidentSalutes      = (currentStage.[3] = 'P')
    let colonelSalutes        = (currentStage.[2] = '3')
    let secondOfficerSalutes  = (currentStage.[1] = '2')
    let firstOfficerSalutes   = (currentStage.[0] = '1')

    let manY       = 100.0F<epx>
    let medalY     =  80.0F<epx>
    let presidentX = 256.0F<epx>
    let colonelX   = 192.0F<epx>
    let secondOffX = 128.0F<epx>
    let firstOffX  =  64.0F<epx>

    let presidentImage = 
        if presidentSalutes           then ImagePresidentSaluting
        else if presidentHoldsOutHand then ImagePresidentPresentingMedal
        else ImagePresidentStanding

    let colonelImage = 
        if colonelSalutes           then ImageColonelSaluting
        else if colonelHoldsOutHand then ImageColonelReceivingMedal
        else ImageColonelStanding

    let medalX =
        if not medalTransferPhase then (colonelX + 10.0F<epx>)
        else if presidentHoldsOutHand || colonelHoldsOutHand then (presidentX + colonelX) / 2.0F
        else presidentX

    let secondOfficerImage =
        if secondOfficerSalutes then ImageSoldierSaluting else ImageSoldierStanding

    let firstOfficerImage =
        if firstOfficerSalutes then ImageSoldierSaluting else ImageSoldierStanding

    let draw cy cx imageId =
        CentreImage render cx cy (ImageFromID imageId)

    draw manY presidentX presidentImage
    draw manY colonelX colonelImage
    draw medalY medalX ImageMedal
    draw manY secondOffX secondOfficerImage
    draw manY firstOffX firstOfficerImage

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderVictoryScreen render (model:VictoryScreenModel) gameTime =
    
    Image1to1 render 0<epx> 0<epx> (VictoryScreenImageID |> ImageFromID)
    
    match model.AnimationStage with
        
        | [] ->
            Text render BlackFontID CentreAlign MiddleAlign (ScreenWidthInt / 2)  (75<epx>) "CONGRATULATIONS"
            Text render RedFontID   CentreAlign MiddleAlign (ScreenWidthInt / 2) (100<epx>) "MISSION COMPLETE"
            Text render BlueFontID  CentreAlign MiddleAlign (ScreenWidthInt / 2) (125<epx>) (model.ScoreText)
            Text render BlueFontID  CentreAlign MiddleAlign (ScreenWidthInt / 2) (150<epx>) (model.HiScoreText)

        | currentStage::_ ->
            RenderAnimationStage render currentStage

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private NextVictoryScreenState gameState keyStateGetter gameTime elapsed =

    let input = keyStateGetter |> DecodedInput
    let model = ModelFrom gameState

    if input.Fire.JustDown then
        model.WhereToAfterCtor model.ScoreAndHiScore gameTime

    else
        let nextAnimStageTime = model.NextAnimStageTime
        let animationStage    = model.AnimationStage   

            
        let nextAnimStageTime , animationStage =
            match animationStage with
                | [] -> 
                    nextAnimStageTime , animationStage

                | _::animationListTail -> 
                    if gameTime > nextAnimStageTime then
                        (gameTime + TimePerAnimStage) , animationListTail
                    else
                        nextAnimStageTime , animationStage


        gameState |> WithUpdatedModel
            {
                ScoreAndHiScore     = model.ScoreAndHiScore   // never changes
                ScoreText           = model.ScoreText         // never changes
                HiScoreText         = model.HiScoreText       // never changes
                NextAnimStageTime   = nextAnimStageTime
                AnimationStage      = animationStage
                WhereToAfterCtor    = model.WhereToAfterCtor  // never changes
            }
        
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewVictoryScreen scoreAndHiScore whereToAfter gameTime =

    let victoryModel =
        {
            ScoreAndHiScore     = scoreAndHiScore
            ScoreText           = "FINAL SCORE   " + scoreAndHiScore.Score.ToString()
            HiScoreText         = "HI SCORE   " + scoreAndHiScore.HiScore.ToString()
            NextAnimStageTime   = gameTime + TimePerAnimStage
            AnimationStage      = AnimationSequence gameTime
            WhereToAfterCtor    = whereToAfter
        }

    NewGameState NextVictoryScreenState RenderVictoryScreen victoryModel

