module SustainModeUntil

open GenericSustain
open Input


let AsInterruptableVideoThen whereToAfter interruptKey ongoingVideo =

    ongoingVideo |> SustainModeUntil (fun gameTime keyStateGetter ->
        if (keyStateGetter interruptKey).JustDown then
            Some (whereToAfter gameTime)
        else
            None
    )


let UntilFutureTimeAndThen futureTime whereToAfter gameState =

    gameState |> SustainModeUntil (fun gameTime _keyStateGetter ->
        if gameTime >= futureTime then
            Some (whereToAfter gameTime)
        else
            None
    )

