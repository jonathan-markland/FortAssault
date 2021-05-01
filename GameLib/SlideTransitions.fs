module SlideTransitions

open Time
open Geometry
open Mechanics
open GameStateManagement
open RenderProjections



type IncomingScreenLocation = 
    ComingFromAbove | ComingFromBelow | ComingFromLeft | ComingFromRight



let private InitialTopLeft secondScreenLocation displayWidth displayHeight =
    match secondScreenLocation with
        | ComingFromAbove -> { ptx =        0<epx> ; pty = -displayHeight }
        | ComingFromBelow -> { ptx =        0<epx> ; pty =  displayHeight }
        | ComingFromLeft  -> { ptx = -displayWidth ; pty =         0<epx> }
        | ComingFromRight -> { ptx =  displayWidth ; pty =         0<epx> }



type private SlideTransitionModel2 =
    {
        SecondScreenRelativePosition : Point<int<epx>>
        PositionFunction             : GameTime -> MOMReason
    }



type private SlideTransitionModel =
    {
        SlideTransitionModel2 : SlideTransitionModel2
        FromScreenState       : ErasedGameState
        ToScreenState         : ErasedGameState
    }



let private RenderSlideTransition render (model:SlideTransitionModel) gameTime =
    
    match model.SlideTransitionModel2.PositionFunction gameTime with
        | MOMYetToAppear -> ()

        | MOMVisibleAtPosition pt -> 
            let pt = pt |> ApplyToPoint RoundF32EpxToIntEpx
            model.FromScreenState.Draw (render |> OffsetBy pt) gameTime
            let ox = model.SlideTransitionModel2.SecondScreenRelativePosition.ptx
            let oy = model.SlideTransitionModel2.SecondScreenRelativePosition.pty
            model.ToScreenState.Draw (render |> OffsetBy (pt |> OffsetByOrigin ox oy)) gameTime

        | MOMDisappeared -> ()



let private NextSlideTransition gameState keyStateGetter gameTime elapsed =

    let slideTransitionModel = 
        ModelFrom gameState

    let { SlideTransitionModel2 = model2 ; FromScreenState=fromState ; ToScreenState=toState } = slideTransitionModel

    match slideTransitionModel.SlideTransitionModel2.PositionFunction gameTime with

        | MOMDisappeared ->
            slideTransitionModel.ToScreenState

        | _ ->
            let fromState = 
                fromState.Frame keyStateGetter gameTime elapsed

            let toState = 
                toState.Frame keyStateGetter gameTime elapsed

            gameState
                |> WithUpdatedModel { SlideTransitionModel2 = model2 ; FromScreenState=fromState ; ToScreenState=toState }



let NewSlideTransition fromScreen toScreen toScreenLocation displayWidth displayHeight motionFunction duration gameTime =

    // The animation path is along a line starting at (0,0) which is the initial
    // location of the top left corner of the fromScreen.  The line ends at the
    // final location of top left corner of the fromScreen.
    //
    // The top left corner of the toScreen is calculated relative to the top
    // left corner of the fromScreen as a fixed offset.

    let toScreenRelativePosition = 
        InitialTopLeft toScreenLocation displayWidth displayHeight

    let startPos = 
        { ptx=0.0F<epx> ; pty=0.0F<epx> }

    let endPos = 
        toScreenRelativePosition |> InvertVector |> ApplyToPoint IntToF32Epx
    
    let slideTransitionModel2 = 
        { 
            SecondScreenRelativePosition = toScreenRelativePosition
            PositionFunction = FunctionThatGetsPositionOfMovingObject motionFunction startPos endPos gameTime duration
        }

    let slideTransitionModel =
        {
            SlideTransitionModel2 = slideTransitionModel2
            FromScreenState       = fromScreen
            ToScreenState         = toScreen
        }

    NewGameState NextSlideTransition RenderSlideTransition slideTransitionModel
