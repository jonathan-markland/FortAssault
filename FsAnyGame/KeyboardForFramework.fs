/// For Web and Desktop.  Intended for use by the Framework in the main event "loop" only.
module KeyboardForFramework

open Input

let BrowserPauseKey = WebBrowserKeyCode 80

// ------------------------------------------------------------------------------------------------------
//  Mutable Key State of an individual key
// ------------------------------------------------------------------------------------------------------

type MutableKeyState<'hostKeyCodeType> =
    {
        WebBrowserKeyCode         : WebBrowserKeyCode
        ThisKeyScanCode           : 'hostKeyCodeType  // TODO: rename to HostKeyCode
        mutable MutHeld           : bool
        mutable MutJustPressed    : bool
        mutable MutWaitingRelease : bool // TODO: Is this functionally the same as MutHeld now?
    }

let NewMutableKey webBrowserKeyCode hostKeyCode =
    {
        WebBrowserKeyCode = webBrowserKeyCode
        ThisKeyScanCode   = hostKeyCode
        MutHeld           = false
        MutJustPressed    = false
        MutWaitingRelease = false
    }

let ToReadOnlyKeyStateRecord mutableKeyState =
    {
        JustDown = mutableKeyState.MutJustPressed
        Held     = mutableKeyState.MutHeld
    }




// ------------------------------------------------------------------------------------------------------
//  Mutable Key State Store for all the keys needed by the game
// ------------------------------------------------------------------------------------------------------

type MutableKeyStateStore<'hostKeyCodeType> =
    {
        MutableKeyStatesList     : MutableKeyState<'hostKeyCodeType> list
        PauseKey                 : MutableKeyState<'hostKeyCodeType>
        mutable GameIsPaused     : bool
    }



/// Query whether the pause flag is set.
let IsGamePaused mutableKeyStateStore =
    mutableKeyStateStore.GameIsPaused



/// Toggle the state of the game-pause flag.
let private TogglePauseMode mutableKeyStateStore =
    mutableKeyStateStore.GameIsPaused 
        <- not mutableKeyStateStore.GameIsPaused



let private LookupHostKey mutableKeyStateStore (hostKeyCode: 'hostKeyCodeType) =

    match mutableKeyStateStore.MutableKeyStatesList 
            |> List.tryFind (fun mutableKey -> mutableKey.ThisKeyScanCode = hostKeyCode) with

        | Some userDefinedKey -> 
            Some userDefinedKey

        | None ->

            // Since the PAUSE key isn't in the user-defined list, we need the following.
            // If it was in the user-defined list, it would have to be exposed into the
            // game engine, even though the game engine doesn't need the PAUSE key.

            if hostKeyCode = mutableKeyStateStore.PauseKey.ThisKeyScanCode then
                Some (mutableKeyStateStore.PauseKey)
            else
                None



/// Constructs a list of mutable records intended to support host frameworks
/// in key-state event recording.  The input list is a list of host-defined
/// key codes, and the return list is MutableKeyState records initialised with 
/// those codes, where the keys are 'not pressed'.  The input list should NOT contain
/// the PAUSE key.
let NewMutableKeyStateStore (pauseKeyHostCode: 'hostKeyCodeType) (hostKeyCodeList: ('hostKeyCodeType * WebBrowserKeyCode) list) =
    {
        MutableKeyStatesList  = hostKeyCodeList |> List.map (fun (host,web) -> NewMutableKey web host)
        PauseKey              = NewMutableKey BrowserPauseKey pauseKeyHostCode
        GameIsPaused          = false
    }



let LiveKeyStateFrom mutableKeyStateStore (keyCode:WebBrowserKeyCode) =

    let inner = mutableKeyStateStore

    match inner.MutableKeyStatesList
            |> List.tryFind (fun mutableKey -> mutableKey.WebBrowserKeyCode = keyCode) with

        | Some userDefinedKey -> 
            userDefinedKey |> ToReadOnlyKeyStateRecord

        | None ->

            // Since the PAUSE key isn't in the user-defined list, we need the following.
            // If it was in the user-defined list, it would have to be exposed into the
            // game engine, even though the game engine doesn't need the PAUSE key.

            if keyCode = inner.PauseKey.WebBrowserKeyCode then
                inner.PauseKey |> ToReadOnlyKeyStateRecord

            else
                // We totally don't recognise this keyCode
                InputEventKeyStateWhereNothingIsPressed
    



/// Mark all of the keys in the list as 'not just pressed', and return
/// an indicator as to whether any of the 'just pressed' states changed
/// as a result of this reset.
let ClearKeyJustPressedFlags mutableKeyStateStore =

    let lst = mutableKeyStateStore.MutableKeyStatesList

    if lst |> List.exists (fun key -> key.MutJustPressed) then
        lst |> List.iter (fun key -> key.MutJustPressed <- false)




/// Handler for the host environment's 'key down' event.
/// Also handles the PAUSE key.
/// This function supports being called on key-repeats for the key in question.
let HandleKeyDownEvent mutableKeyStateStore (hostKeyCode: 'hostKeyCodeType) =

    let store = mutableKeyStateStore

    let processKey key =
        if not key.MutWaitingRelease then
            if key.ThisKeyScanCode = store.PauseKey.ThisKeyScanCode then
                TogglePauseMode mutableKeyStateStore
            key.MutHeld <- true
            key.MutJustPressed    <- true
            key.MutWaitingRelease <- true

    match LookupHostKey store hostKeyCode with
        | None      -> false // because we didn't recognise the key!
        | Some key  -> processKey key ; true



/// Handler for the host environment's 'key up' event.
let HandleKeyUpEvent mutableKeyStateStore (hostKeyCode: 'hostKeyCodeType) =

    let store = mutableKeyStateStore

    match LookupHostKey store hostKeyCode with
        
        | None -> 
            false // because we didn't recognise the key, which would include PAUSE, but there's no key-up action for PAUSE.
        
        | Some key ->
            key.MutHeld           <- false
            key.MutWaitingRelease <- false
            true

