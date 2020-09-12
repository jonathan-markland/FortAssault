module InputForFramework

open InputEventData
open KeyboardForFramework


        // I did not want to make this visible to all client of the InputEventData module
        // because it's only for the framework's message-loop, and involves mutability.


/// Snapshot the key states into an immutable record type.
/// This is specific to Fort Assault.
/// This doesn't include PAUSE because that's not visible within the game engine.
let ObtainFortAssaultKeyStatesAsImmutableRecordFrom mutableKeyStateStore =

    match mutableKeyStateStore.MutableKeyStatesList with

        | [ keyLeft ; keyRight ; keyUp ; keyDown ; keyFire ] ->
            {
                Left =
                    {
                        JustDown  = keyLeft.MutJustPressed
                        Held      = keyLeft.MutHeld
                    }
                Right =
                    {
                        JustDown = keyRight.MutJustPressed
                        Held     = keyRight.MutHeld
                    }
                Up =
                    {
                        JustDown = keyUp.MutJustPressed
                        Held     = keyUp.MutHeld
                    }
                Down =
                    {
                        JustDown = keyDown.MutJustPressed
                        Held     = keyDown.MutHeld
                    }
                Fire =
                    {
                        JustDown = keyFire.MutJustPressed
                        Held     = keyFire.MutHeld
                    }
            }
        | _ -> failwith "Immutable key record needs extending"

