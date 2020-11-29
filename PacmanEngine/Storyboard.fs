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
open Geometry
open ScreenHandler

type BlankScreenModel =
    {
        ScreenColour : SolidColour
    }

let RenderBlankScreen render (model:BlankScreenModel) gameTime =
    render (DrawFilledRectangle (0<epx>, 0<epx>, ScreenWidthInt, ScreenHeightInt, model.ScreenColour))

let NewBlankScreen colour =
    let blankScreenModel = { ScreenColour = colour }
    NewGameState ModelNeverChanges RenderBlankScreen blankScreenModel

// - - - - 

let OffsetBy (pt:Point<int<epx>>) render =

    let { ptx=ofx ; pty=ofy } = pt

    let renderOffsetFunc drawingShape =
        render 
            (match drawingShape with

            | DrawImageWithTopLeftAtInt (x,y,img) -> 
                DrawImageWithTopLeftAtInt (x+ofx,y+ofy,img)

            | DrawFilledRectangle (x,y,w,h,clr) ->
                DrawFilledRectangle (x+ofx,y+ofy,w,h,clr)

            | DrawStretchedImageWithTopLeftAt (x,y,img,w,h) -> 
                DrawStretchedImageWithTopLeftAt (
                    (ofx + (x |> FloatEpxToIntEpx)) |> IntToFloatEpx,
                    (ofy + (y |> FloatEpxToIntEpx)) |> IntToFloatEpx,
                    img,w,h)

            | DrawSubImageStretchedToTarget (sx,sy,sw,sh,dx,dy,dw,dh,img) ->
                DrawSubImageStretchedToTarget (
                    (ofx |> IntEpxToInt) + sx,
                    (ofy |> IntEpxToInt) + sy,
                    sw,sh,
                    (ofx + (dx |> FloatEpxToIntEpx)) |> IntToFloatEpx,
                    (ofy + (dy |> FloatEpxToIntEpx)) |> IntToFloatEpx,
                    dw,dh,img)
            )

    renderOffsetFunc


open Mechanics
open Time

type IncomingScreenLocation = ComingFromAbove | ComingFromBelow | ComingFromLeft | ComingFromRight

let InitialTopLeft secondScreenLocation displayWidth displayHeight =
    match secondScreenLocation with
        | ComingFromAbove -> { ptx =        0<epx> ; pty = -displayHeight }
        | ComingFromBelow -> { ptx =        0<epx> ; pty =  displayHeight }
        | ComingFromLeft  -> { ptx = -displayWidth ; pty =         0<epx> }
        | ComingFromRight -> { ptx =  displayWidth ; pty =         0<epx> }

type SlideTransitionModel2 =
    {
        SecondScreenRelativePosition : Point<int<epx>>
        PositionFunction             : float32<seconds> -> MOMReason
        WhereToAfter                 : float32<seconds> -> ErasedGameState
    }

type SlideTransitionModel =
    {
        SlideTransitionModel2 : SlideTransitionModel2
        FromScreenState       : ErasedGameState
        ToScreenState         : ErasedGameState
    }

let RenderSlideTransition render (model:SlideTransitionModel) gameTime =
    
    match model.SlideTransitionModel2.PositionFunction gameTime with
        | MOMYetToAppear -> ()

        | MOMVisibleAtPosition pt -> 
            let pt = pt |> ApplyToPoint FloatEpxToIntEpx
            model.FromScreenState.Draw (render |> OffsetBy pt) gameTime
            let ox = model.SlideTransitionModel2.SecondScreenRelativePosition.ptx
            let oy = model.SlideTransitionModel2.SecondScreenRelativePosition.pty
            model.ToScreenState.Draw (render |> OffsetBy (pt |> OffsetByOrigin ox oy)) gameTime

        | MOMDisappeared -> ()


let NextSlideTransition gameState keyStateGetter gameTime elapsed =

    let slideTransitionModel = ModelFrom gameState
    let { SlideTransitionModel2 = model2 ; FromScreenState=fromState ; ToScreenState=toState } = slideTransitionModel

    match slideTransitionModel.SlideTransitionModel2.PositionFunction gameTime with

        | MOMDisappeared ->
            slideTransitionModel.SlideTransitionModel2.WhereToAfter gameTime

        | _ ->
            let fromState = fromState.Frame keyStateGetter gameTime elapsed
            let toState   = toState.Frame keyStateGetter gameTime elapsed
            gameState
                |> WithUpdatedModel { SlideTransitionModel2 = model2 ; FromScreenState=fromState ; ToScreenState=toState }


let NewSlideTransition fromScreen toScreen toScreenLocation displayWidth displayHeight motionFunction duration gameTime whereToAfter =

    // The animation path is along a line starting at (0,0) which is the initial
    // location of the top left corner of the fromScreen.  The line ends at the
    // final location of top left corner of the fromScreen.
    //
    // The top left corner of the toScreen is calculated relative to the top
    // left corner of the fromScreen as a fixed offset.

    let toScreenRelativePosition = 
        InitialTopLeft toScreenLocation displayWidth displayHeight

    let startPos = { ptx=0.0F<epx> ; pty=0.0F<epx> }
    let endPos   = toScreenRelativePosition |> InvertVector |> ApplyToPoint IntToFloatEpx
    
    let slideTransitionModel2 = 
        { 
            SecondScreenRelativePosition = toScreenRelativePosition
            PositionFunction = FunctionThatGetsPositionOfMovingObject motionFunction startPos endPos gameTime duration
            WhereToAfter = whereToAfter
        }

    let slideTransitionModel =
        {
            SlideTransitionModel2 = slideTransitionModel2
            FromScreenState       = fromScreen
            ToScreenState         = toScreen
        }

    NewGameState NextSlideTransition RenderSlideTransition slideTransitionModel


    

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let NewPacmanStoryboard _ gameTime =

    // globalScoreboard <- InitialScoreboard [ "DAMON" ; "JARVIS" ; "NOELLE" ; "PAT" ] 1000u 2000u
    // 
    // GameTitleStory gameTime
    
    let magentaScreen = NewBlankScreen (SolidColour 0xFF00FFu)
    let yellowScreen  = NewBlankScreen (SolidColour 0xFFFF30u)
    let redScreen     = NewBlankScreen (SolidColour 0xFF0000u)

    let whereToAfter _ = redScreen

    NewSlideTransition 
        magentaScreen yellowScreen ComingFromRight
        ScreenWidthInt ScreenHeightInt
        SlowingDownMotion 1.0F<seconds> gameTime 
        whereToAfter

