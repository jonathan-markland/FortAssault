module ScreenVictory

open Time
open SharedDrawing
open DrawingCommandsEx
open ScoreHiScore
open FontAlignment
open Geometry
open InputEventData
open StoryboardChapters
open ImagesAndFonts

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type VictoryScreenModel =
    {
        ScoreAndHiScore     : ScoreAndHiScore
        ScoreText           : string
        HiScoreText         : string
        NextAnimStageTime   : float32<seconds>
        AnimationStage      : string list
        RestartNow          : bool
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

let TimePerAnimStage = 1.0F<seconds>

//           |v---- P = President holds out hand  | Either of these implies showing the medal in the hands position.
//           | v--- C = Colonel holds out hand    | 
//           | 
//           |  v-- x = Medal transfer phase

let MedalSequence =
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

let SaluteSequence =
    [
        "---P|---"
        "123P|---"
        "123P|---"
        "123P|---"
        "123-|---"
    ]

let MickeySaluteSequence =
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

let pauseAfterwardsSequence =
    [
        "----|---"
        "----|---"
        "----|---"
        "----|---"
    ]

let NormalAnimation = MedalSequence @ SaluteSequence       @ pauseAfterwardsSequence
let MickeyAnimation = MedalSequence @ MickeySaluteSequence @ pauseAfterwardsSequence

let AnimationSequence gameTime =
    if (gameTime % 4.0F<seconds>) < 1.0F<seconds> then MickeyAnimation else NormalAnimation

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderAnimationStage render (currentStage:string) =

    let medalTransferPhase    = (currentStage.[7] = 'x')
    let colonelHoldsOutHand   = (currentStage.[6] = 'C')
    let presidentHoldsOutHand = (currentStage.[5] = 'P')
    let presidentSalutes      = (currentStage.[3] = 'P')
    let colonelSalutes        = (currentStage.[2] = '3')
    let secondOfficerSalutes  = (currentStage.[1] = '2')
    let firstOfficerSalutes   = (currentStage.[0] = '1')

    let cy = 100.F<wu>
    let my =  80.0F<wu>

    // President

    if presidentSalutes then
        CentreImage render 256.0F<wu> cy ImagePresidentSaluting
    else if presidentHoldsOutHand then
        CentreImage render 256.0F<wu> cy ImagePresidentPresentingMedal
    else 
        CentreImage render 256.0F<wu> cy ImagePresidentStanding

    // Colonel

    if colonelSalutes then
        CentreImage render 192.0F<wu> cy ImageColonelSaluting
    else if colonelHoldsOutHand then
        CentreImage render 192.0F<wu> cy ImageColonelReceivingMedal
    else
        CentreImage render 192.0F<wu> cy ImageColonelStanding

    // Medal

    if not medalTransferPhase then
        CentreImage render 192.0F<wu> my ImageMedal
    else if presidentHoldsOutHand || colonelHoldsOutHand then
        CentreImage render 224.0F<wu> my ImageMedal
    else
        CentreImage render 256.0F<wu> my ImageMedal

    // Second officer    

    if secondOfficerSalutes then
        CentreImage render 128.0F<wu> cy ImageSoldierSaluting
    else
        CentreImage render 128.0F<wu> cy ImageSoldierStanding

    // First officer

    if firstOfficerSalutes then
        CentreImage render 64.0F<wu> cy ImageSoldierSaluting
    else
        CentreImage render 64.0F<wu> cy ImageSoldierStanding

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let RenderVictoryScreen render (model:VictoryScreenModel) gameTime =
    
    Image1to1 render 0<wu> 0<wu> VictoryScreenImageID
    
    match model.AnimationStage with
        
        | [] ->
            Text render BlackFontID CentreAlign MiddleAlign (ScreenWidthInt / 2)  (75<wu>) "CONGRATULATIONS"
            Text render RedFontID   CentreAlign MiddleAlign (ScreenWidthInt / 2) (100<wu>) "MISSION COMPLETE"
            Text render BlueFontID  CentreAlign MiddleAlign (ScreenWidthInt / 2) (125<wu>) (model.ScoreText)
            Text render BlueFontID  CentreAlign MiddleAlign (ScreenWidthInt / 2) (150<wu>) (model.HiScoreText)

        | currentStage::_ ->
            RenderAnimationStage render currentStage

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewVictoryScreen scoreAndHiScore gameTime =
    {
        ScoreAndHiScore     = scoreAndHiScore
        ScoreText           = "FINAL SCORE   " + scoreAndHiScore.Score.ToString()
        HiScoreText         = "HI SCORE   " + scoreAndHiScore.HiScore.ToString()
        NextAnimStageTime   = gameTime + TimePerAnimStage
        AnimationStage      = AnimationSequence gameTime
        RestartNow          = false
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NextVictoryScreenState oldState input gameTime =

    let newModel =
        if input.Fire.JustDown then
            { oldState with RestartNow = true }
        else
            let nextAnimStageTime = oldState.NextAnimStageTime
            let animationStage    = oldState.AnimationStage   

            
            let nextAnimStageTime , animationStage =
                match animationStage with
                    | [] -> 
                        nextAnimStageTime , animationStage

                    | _::animationListTail -> 
                        if gameTime > nextAnimStageTime then
                            (gameTime + TimePerAnimStage) , animationListTail
                        else
                            nextAnimStageTime , animationStage


            {
                ScoreAndHiScore     = oldState.ScoreAndHiScore   // never changes
                ScoreText           = oldState.ScoreText         // never changes
                HiScoreText         = oldState.HiScoreText       // never changes
                NextAnimStageTime   = nextAnimStageTime
                AnimationStage      = animationStage
                RestartNow          = oldState.RestartNow        // never changes
            }


    if newModel.RestartNow then
        GoToNextChapter1(newModel)
    else
        StayOnThisChapter1(newModel)
        