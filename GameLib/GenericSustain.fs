module GenericSustain

open Time
open GameStateManagement



type private SustainModel =
    {
        GuestState           : ErasedGameState
        StateSwitchChooser   : float32<seconds> -> KeyStateFunction -> ErasedGameState option
    }



/// We just pass rendering requests straight through to the guest state.
let private RenderGuestState render model (gameTime:float32<seconds>) =
    model.GuestState.Draw render gameTime



/// Decides if we're still sustaining, or switching.
let private DecideNextState gameState keyStateGetter gameTime elapsed =

    let model = ModelFrom gameState

    match model.StateSwitchChooser gameTime keyStateGetter with

        | Some newGameState ->
            newGameState

        | None ->   
            gameState |> WithUpdatedModel 
                { model with GuestState = model.GuestState.Frame keyStateGetter gameTime elapsed }
        



/// Sustains updates and rendering for the given initialGuestGameState
/// until a condition (defined by the stateSwitchChooser) is met, whereupon
/// we are replaced by the game state returned by the chooser (handover).
let SustainModeUntil stateSwitchChooser initialGuestGameState =

    let model =
        {
            GuestState         = initialGuestGameState
            StateSwitchChooser = stateSwitchChooser
        }

    NewGameState DecideNextState RenderGuestState model

