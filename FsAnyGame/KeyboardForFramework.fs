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
        mutable MutWaitingRelease : bool // TODO: Is this functionally the same as MutHeld now?
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

type MutableKeyStateStoreInner<'hostKeyCodeType> =
    {
        MutableKeyStatesList     : MutableKeyState<'hostKeyCodeType> list
        PauseKey                 : MutableKeyState<'hostKeyCodeType>
        mutable GameIsPaused     : bool
    }

type MutableKeyStateStore<'hostKeyCodeType, 'userDefinedImmutableKeysRecord> =
    {
        SystemKeyStateStore            : MutableKeyStateStoreInner<'hostKeyCodeType>
        mutable UserKeyStatesSnapshot  : 'userDefinedImmutableKeysRecord
        SnapshotObtainer               : MutableKeyStateStoreInner<'hostKeyCodeType> -> 'userDefinedImmutableKeysRecord
    }



/// Obtain an immutable record of the key states specific to the client game.
let ImmutableKeyStatesSnapshot mutableKeyStateStore =
    mutableKeyStateStore.UserKeyStatesSnapshot  // To reduce garbage we re-build this ONLY on-change.



/// Query whether the pause flag is set.
let IsGamePaused mutableKeyStateStore =
    mutableKeyStateStore.SystemKeyStateStore.GameIsPaused



/// Toggle the state of the game-pause flag.
let private TogglePauseMode mutableKeyStateStore =
    mutableKeyStateStore.SystemKeyStateStore.GameIsPaused 
        <- not mutableKeyStateStore.SystemKeyStateStore.GameIsPaused



let private LookupRecordForKey mutableKeyStateStore (hostKeyCode: 'hostKeyCodeType) =

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
let NewMutableKeyStateStore userKeyStatesSnaphotObtainer (pauseKeyHostCode: 'hostKeyCodeType) (hostKeyCodeList: 'hostKeyCodeType list) =
    let mutableKeyStateStoreInner =
        {
            MutableKeyStatesList  = hostKeyCodeList |> List.map NewMutableKey
            PauseKey              = NewMutableKey pauseKeyHostCode
            GameIsPaused          = false
        }
    let outer =
        {
            SystemKeyStateStore   = mutableKeyStateStoreInner
            UserKeyStatesSnapshot = mutableKeyStateStoreInner |> userKeyStatesSnaphotObtainer
            SnapshotObtainer      = userKeyStatesSnaphotObtainer
        }
    outer



let private HandlePossibleKeyStateChange mutableKeyStateStore keyOperationResult =

    // Garbage avoidance scheme.

    match keyOperationResult with

        | KeyStateChanged   -> 
            mutableKeyStateStore.UserKeyStatesSnapshot
                <- mutableKeyStateStore.SnapshotObtainer mutableKeyStateStore.SystemKeyStateStore

        | NoKeyStateChanged -> 
            ()



/// Mark all of the keys in the list as 'not just pressed', and return
/// an indicator as to whether any of the 'just pressed' states changed
/// as a result of this reset.
let ClearKeyJustPressedFlags mutableKeyStateStore =

    let wasChanged = 

        let keyListReference = mutableKeyStateStore.SystemKeyStateStore.MutableKeyStatesList

        if keyListReference |> List.exists (fun mutableKey -> mutableKey.MutJustPressed) then
            keyListReference |> List.iter (fun mutableKey -> mutableKey.MutJustPressed <- false)
            KeyStateChanged

        else
            NoKeyStateChanged

    HandlePossibleKeyStateChange mutableKeyStateStore wasChanged



/// Handler for the host environment's 'key down' event.
/// Also handles the PAUSE key.
/// This function supports being called on key-repeats for the key in question.
let HandleKeyDownEvent mutableKeyStateStore (hostKeyCode: 'hostKeyCodeType) =

    let inner = mutableKeyStateStore.SystemKeyStateStore

    let processKey mutableKey =
        if mutableKey.MutWaitingRelease then
            NoKeyStateChanged  // <-- key repeat detected, we must not 'reset' JustPressed or KeyDownSince!

        else 
            if mutableKey.ThisKeyScanCode = inner.PauseKey.ThisKeyScanCode then
                TogglePauseMode mutableKeyStateStore
            mutableKey.MutHeld <- true
            mutableKey.MutJustPressed    <- true
            mutableKey.MutWaitingRelease <- true
            KeyStateChanged

    let wasChanged =
        match LookupRecordForKey inner hostKeyCode with
            | None             -> NoKeyStateChanged // because we didn't recognise the key!
            | Some(mutableKey) -> processKey mutableKey

    HandlePossibleKeyStateChange mutableKeyStateStore wasChanged
    wasChanged


/// Handler for the host environment's 'key up' event.
let HandleKeyUpEvent mutableKeyStateStore (hostKeyCode: 'hostKeyCodeType) =

    let inner = mutableKeyStateStore.SystemKeyStateStore

    let wasChanged =
        match LookupRecordForKey inner hostKeyCode with
        
            | None -> 
                NoKeyStateChanged // because we didn't recognise the key, which would include PAUSE, but there's no key-up action for PAUSE.
        
            | Some mutableKey ->
                mutableKey.MutHeld           <- false
                mutableKey.MutWaitingRelease <- false
                KeyStateChanged

    HandlePossibleKeyStateChange mutableKeyStateStore wasChanged
    wasChanged

