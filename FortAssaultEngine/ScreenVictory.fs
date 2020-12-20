﻿module ScreenVictory

open Time
open DrawingFunctions
open ScoreHiScore
open Geometry
open InputEventData
open ResourceIDs
open ImagesAndFonts
open StaticResourceAccess
open ScreenHandler

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type private VictoryScreenModel =
    {
        ScoreAndHiScore     : ScoreAndHiScore
        ScoreText           : string
        HiScoreText         : string
        NextAnimStageTime   : float32<seconds>
        AnimationStage      : string list
        WhereToAfterCtor    : ScoreAndHiScore -> float32<seconds> -> ErasedGameState
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

let private TimePerAnimStage = 1.0F<seconds>

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
    if (gameTime % 4.0F<seconds>) < 1.0F<seconds> then MickeyAnimation else NormalAnimation

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private RenderAnimationStage render (currentStage:string) =

    let CentreImage render cx cy img = CentreImage render cx cy (img |> ImageFromID)

    let medalTransferPhase    = (currentStage.[7] = 'x')
    let colonelHoldsOutHand   = (currentStage.[6] = 'C')
    let presidentHoldsOutHand = (currentStage.[5] = 'P')
    let presidentSalutes      = (currentStage.[3] = 'P')
    let colonelSalutes        = (currentStage.[2] = '3')
    let secondOfficerSalutes  = (currentStage.[1] = '2')
    let firstOfficerSalutes   = (currentStage.[0] = '1')

    let cy = 100.F<epx>
    let my =  80.0F<epx>

    // President

    if presidentSalutes then
        CentreImage render 256.0F<epx> cy ImagePresidentSaluting
    else if presidentHoldsOutHand then
        CentreImage render 256.0F<epx> cy ImagePresidentPresentingMedal
    else 
        CentreImage render 256.0F<epx> cy ImagePresidentStanding

    // Colonel

    if colonelSalutes then
        CentreImage render 192.0F<epx> cy ImageColonelSaluting
    else if colonelHoldsOutHand then
        CentreImage render 192.0F<epx> cy ImageColonelReceivingMedal
    else
        CentreImage render 192.0F<epx> cy ImageColonelStanding

    // Medal

    if not medalTransferPhase then
        CentreImage render 192.0F<epx> my ImageMedal
    else if presidentHoldsOutHand || colonelHoldsOutHand then
        CentreImage render 224.0F<epx> my ImageMedal
    else
        CentreImage render 256.0F<epx> my ImageMedal

    // Second officer    

    if secondOfficerSalutes then
        CentreImage render 128.0F<epx> cy ImageSoldierSaluting
    else
        CentreImage render 128.0F<epx> cy ImageSoldierStanding

    // First officer

    if firstOfficerSalutes then
        CentreImage render 64.0F<epx> cy ImageSoldierSaluting
    else
        CentreImage render 64.0F<epx> cy ImageSoldierStanding

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
