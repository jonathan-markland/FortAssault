module FreezeFrame

open Time
open GameStateManagement
open Mechanics



type private FreezeFrameModel =
    {
        FrozenGameState   : ErasedGameState
        TimeFiddler       : float32<seconds> -> float32<seconds>
        UnfreezeGameTime  : float32<seconds>
        PostFreezeCtor    : ErasedGameState -> float32<seconds> -> ErasedGameState
    }



let private RenderFreezeFrame render model (gameTime:float32<seconds>) =
    
    model.FrozenGameState.Draw render (gameTime |> model.TimeFiddler)



let private NextFreezeFrameState gameState _keyStateGetter gameTime _elapsed =
    
    let model = ModelFrom gameState
    
    if gameTime < model.UnfreezeGameTime then
        Unchanged gameState
    else
        model.PostFreezeCtor (model.FrozenGameState |> WithoutAnyFurtherUpdates) gameTime



let AdaptedToIgnoreOutgoingStateParameter (func:float32<seconds> -> ErasedGameState) =
    let adapter (_outgoingState:ErasedGameState) (gameTime:float32<seconds>) =
        func gameTime
    adapter



// TODO: Review this for ScreenHandler.FrozenInTimeAt and ScreenHandler.WithoutAnyFurtherUpdates



/// The outgoingGameState is frozen at the current gameTime.
/// The returned ErasedGameState will hold that image for the duration specified
/// before calling the 'whereToAfter' constructor, quoting the most recent
/// state, to establish the caller's desired next state.
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



let WithOneShotSound oneShotSoundOperations (innerGameState:ErasedGameState) =

    let frameFunc _gameState keyStateGetter gameTime elapsed =
        // Just delegate the call, and whatever innerGameState returns 
        // will naturally replace us on the next frame:
        innerGameState.Frame keyStateGetter gameTime elapsed  

    let drawFunc render model (gameTime:float32<seconds>) =
        // Just delegate drawing.
        // This will only be called once because of the replacement done by nextFrame
        innerGameState.Draw render gameTime

    NewGameStateAndSounds frameFunc drawFunc () oneShotSoundOperations
            
    