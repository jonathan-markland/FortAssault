module InterruptableVideo

open Time
open ScreenHandler
open Input



type private InterruptableVideoModel =
    {
        InterruptKey       : WebBrowserKeyCode
        VideoPlayerState   : ErasedGameState
        WhereToOnInterrupt : float32<seconds> -> ErasedGameState
    }



let private RenderInterruptable render model (gameTime:float32<seconds>) =
    
    model.VideoPlayerState.Draw render gameTime



let private NextInterruptableState gameState (keyStateGetter:WebBrowserKeyCode -> InputEventKeyState) gameTime elapsed =

    let model = ModelFrom gameState

    if (keyStateGetter model.InterruptKey).JustDown then
        model.WhereToOnInterrupt gameTime
    else
        gameState |> WithUpdatedModel 
            { model with VideoPlayerState = model.VideoPlayerState.Frame KeyStatesInhibited gameTime elapsed }
        


let AsInterruptableVideoThen whereToAfter interruptKey ongoingVideo =

    let interruptableVideoModel =
        {
            InterruptKey       = interruptKey
            VideoPlayerState   = ongoingVideo
            WhereToOnInterrupt = whereToAfter
        }

    NewGameState NextInterruptableState RenderInterruptable interruptableVideoModel

