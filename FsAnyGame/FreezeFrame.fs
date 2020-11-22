module FreezeFrame

open Time
open ScreenHandler
open Mechanics



type private FreezeFrameModel =
    {
        FrozenGameState   : ErasedGameState
        TimeFiddler       : float32<seconds> -> float32<seconds>
        UnfreezeGameTime  : float32<seconds>
        PostFreezeCtor    : float32<seconds> -> ErasedGameState
    }



let private RenderFreezeFrame render model (gameTime:float32<seconds>) =
    
    model.FrozenGameState.Draw render (gameTime |> model.TimeFiddler)



let private NextFreezeFrameState gameState keyStateGetter gameTime elapsed =
    
    let model = ModelFrom gameState
    
    if gameTime < model.UnfreezeGameTime then
        Unchanged gameState
    else
        model.PostFreezeCtor gameTime



/// The outgoingGameState is frozen at the current gameTime.
/// The returned ErasedGameState will hold that image for the duration specified
/// before calling the 'whereToAfter' constructor to establish the next state.
let WithFreezeFrameFor duration gameTime whereToAfter outgoingGameState =

    let freezeModel =
        {
            FrozenGameState  = outgoingGameState
            TimeFiddler      = (fun _ -> gameTime)   // Freeze time for the outgoingGameState at the time right now.
            UnfreezeGameTime = gameTime + duration
            PostFreezeCtor   = whereToAfter
        }

    NewGameState NextFreezeFrameState RenderFreezeFrame freezeModel



/// An artistic alternative to just freezing the frame.
/// The outgoingGameState will see no further state update calls, but its drawing
/// function will be called with elapsing time for the duration specified.
let WithDrawingOnlyFor duration gameTime whereToAfter outgoingGameState =

    let freezeModel =
        {
            FrozenGameState  = outgoingGameState
            TimeFiddler      = id
            UnfreezeGameTime = gameTime + duration
            PostFreezeCtor   = whereToAfter
        }

    NewGameState NextFreezeFrameState RenderFreezeFrame freezeModel

