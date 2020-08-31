/// Intended for use in the main event loop only.
module Keyboard

type MutableKeyState<'scanCodeType> =
    {
        ThisKeyScanCode           : 'scanCodeType
        mutable MutHeld           : bool
        mutable MutJustPressed    : bool
        mutable MutWaitingRelease : bool
    }

let NewMutableKey hostKeyCode =
    {
        ThisKeyScanCode   = hostKeyCode
        MutHeld           = false
        MutJustPressed    = false
        MutWaitingRelease = false
    }

let ScanCodeToMutableKeyRecord keysList scanCode =
    keysList |> List.tryFind (fun mutableKey -> mutableKey.ThisKeyScanCode = scanCode)

type KeyStateChangeNotification = NoKeyStateChanged | KeyStateChanged

let ClearKeyJustPressedFlags keysList =

    if keysList |> List.exists (fun mutableKey -> mutableKey.MutJustPressed) then
        keysList |> List.iter (fun mutableKey -> mutableKey.MutJustPressed <- false)
        KeyStateChanged
    else
        NoKeyStateChanged

let HandleKeyDownEvent keysList scancode =

    // This function is required to support being called on key-repeats!

    match ScanCodeToMutableKeyRecord keysList scancode with
        | None -> NoKeyStateChanged // because we didn't recognise the key!
        | Some(mutableKey) ->
            if mutableKey.MutWaitingRelease 
            then NoKeyStateChanged  // <-- key repeat detected, we must not 'reset' JustPressed or KeyDownSince!
            else 
                mutableKey.MutHeld <- true
                mutableKey.MutJustPressed    <- true
                mutableKey.MutWaitingRelease <- true
                KeyStateChanged

let HandleKeyUpEvent keysList scancode =
    match ScanCodeToMutableKeyRecord keysList scancode with
        | None -> NoKeyStateChanged // because we didn't recognise the key!
        | Some(mutableKey) ->
            mutableKey.MutHeld           <- false
            mutableKey.MutWaitingRelease <- false
            KeyStateChanged
