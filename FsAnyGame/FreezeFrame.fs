module FreezeFrame

open Time
open ScreenHandler

type private FreezeFrameModel =
    {
        FrozenGameState   : ErasedGameState
        UnfreezeGameTime  : float32<seconds>
        PostFreezeCtor    : float32<seconds> -> ErasedGameState
    }


let private RenderFreezeFrame render model (gameTime:float32<seconds>) =
    model.FrozenGameState.Draw render gameTime


let private NextFreezeFrameState gameState keyStateGetter gameTime elapsed =
    let model = ModelFrom gameState
    if gameTime < model.UnfreezeGameTime then
        Unchanged gameState
    else
        model.PostFreezeCtor gameTime


let WithFreezeFrameFor duration gameTime whereAfter outgoingGameState =
    let freezeModel =
        {
            FrozenGameState  = outgoingGameState
            UnfreezeGameTime = gameTime + duration
            PostFreezeCtor   = whereAfter
        }
    NewGameState NextFreezeFrameState RenderFreezeFrame freezeModel

