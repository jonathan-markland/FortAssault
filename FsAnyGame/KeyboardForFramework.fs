/// Intended for use by the Framework in the main event loop only.
module KeyboardForFramework

type KeyStateChangeNotification = 
    NoKeyStateChanged | KeyStateChanged

// ------------------------------------------------------------------------------------------------------
//  Mutable Key State of an individual key
// ------------------------------------------------------------------------------------------------------

type MutableKeyState<'hostKeyCodeType> =
    {
        ThisKeyScanCode           : 'hostKeyCodeType  // TODO: rename to HostKeyCode
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

// ------------------------------------------------------------------------------------------------------
//  Mutable Key State Store for all the keys needed by the game
// ------------------------------------------------------------------------------------------------------

type MutableKeyStateStore<'hostKeyCodeType> =
    {
        PauseKeyHostCode         : 'hostKeyCodeType
        mutable GameIsPaused     : bool
        MutableKeyStatesList     : MutableKeyState<'hostKeyCodeType> list
    }



let private LookupRecordForKey keysList (hostKeyCode: 'hostKeyCodeType) =
    keysList 
        |> List.tryFind (fun mutableKey -> mutableKey.ThisKeyScanCode = hostKeyCode)



/// Constructs a list of mutable records intended to support host frameworks
/// in key-state event recording.  The input list is a list of host-defined
/// key codes, and the return list is MutableKeyState records initialised with 
/// those codes, where the keys are 'not pressed'.
let NewMutableKeyStateStore (pauseKeyHostCode: 'hostKeyCodeType) (hostKeyCodeList: 'hostKeyCodeType list) =
    {
        PauseKeyHostCode     = pauseKeyHostCode
        GameIsPaused         = false
        MutableKeyStatesList = hostKeyCodeList |> List.map NewMutableKey
    }



/// Query whether the pause flag is set.
let IsGamePaused mutableKeyStateStore =
    mutableKeyStateStore.GameIsPaused



/// Toggle the state of the game-pause flag.
let TogglePauseMode mutableKeyStateStore =
    mutableKeyStateStore.GameIsPaused <- not mutableKeyStateStore.GameIsPaused



/// Mark all of the keys in the list as 'not just pressed', and return
/// an indicator as to whether any of the 'just pressed' states changed
/// as a result of this reset.
let ClearKeyJustPressedFlags mutableKeyStateStore =
    let keyListReference = mutableKeyStateStore.MutableKeyStatesList
    if keyListReference |> List.exists (fun mutableKey -> mutableKey.MutJustPressed) then
        keyListReference |> List.iter (fun mutableKey -> mutableKey.MutJustPressed <- false)
        KeyStateChanged

    else
        NoKeyStateChanged



/// Handler for the host environment's 'key down' event.
/// This function supports being called on key-repeats for the key in question.
let HandleKeyDownEvent mutableKeyStateStore (hostKeyCode: 'hostKeyCodeType) =

    if hostKeyCode = mutableKeyStateStore.PauseKeyHostCode then

        TogglePauseMode mutableKeyStateStore
        NoKeyStateChanged  // because the PAUSE key doesn't count-- it isn't visible to the game.

    else

        match LookupRecordForKey mutableKeyStateStore.MutableKeyStatesList hostKeyCode with
        
            | None ->
                NoKeyStateChanged // because we didn't recognise the key!
        
            | Some(mutableKey) ->

                if mutableKey.MutWaitingRelease then
                    NoKeyStateChanged  // <-- key repeat detected, we must not 'reset' JustPressed or KeyDownSince!
            
                else 
                    mutableKey.MutHeld <- true
                    mutableKey.MutJustPressed    <- true
                    mutableKey.MutWaitingRelease <- true
                    KeyStateChanged



/// Handler for the host environment's 'key up' event.
let HandleKeyUpEvent mutableKeyStateStore (hostKeyCode: 'hostKeyCodeType) =

    match LookupRecordForKey mutableKeyStateStore.MutableKeyStatesList hostKeyCode with
        
        | None -> 
            NoKeyStateChanged // because we didn't recognise the key, which would include PAUSE, but there's no key-up action for PAUSE.
        
        | Some mutableKey ->
            mutableKey.MutHeld           <- false
            mutableKey.MutWaitingRelease <- false
            KeyStateChanged

