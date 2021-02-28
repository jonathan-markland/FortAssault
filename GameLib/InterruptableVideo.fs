module InterruptableVideo

open SustainModeUntil
open Input


let AsInterruptableVideoThen whereToAfter interruptKey ongoingVideo =

    ongoingVideo |> SustainModeUntil (fun gameTime keyStateGetter ->
        if (keyStateGetter interruptKey).JustDown then
            Some (whereToAfter gameTime)
        else
            None
    )

