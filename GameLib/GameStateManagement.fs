module GameStateManagement

open Time
open DrawingShapes
open Input
open Sounds


/// A user-defined effectful function that processes a primitive drawing instruction.
type RenderFunction = DrawingShapes -> unit


/// A user-defined function that obtains the state of the given keyboard key.
/// Web browser key codes are used as a "common currency" across desktop and web frameworks.
type KeyStateFunction = WebBrowserKeyCode -> InputEventKeyState


/// A container for the game state, with two associated functions, one to draw the
/// frame, and one to calculate the next frame.  "Erased" refers to the fact that
/// the exact type of the game state model is hidden.
[<AbstractClass>] 
type ErasedGameState() =

    /// Draw the model onto the screen, for the given game time, via the given 
    /// render function.
    abstract member Draw  : RenderFunction -> GameTime -> unit

    /// Calculate the model value, Draw function and Frame function for the next 
    /// frame of animation.
    abstract member Frame : KeyStateFunction -> GameTime -> GameTime -> ErasedGameState   // TODO: I never really wanted the framework to pass in the frame time delta.

    /// Obtain the sound commands to be executed for this frame (if any).
    abstract member Sounds : unit -> SoundOperation list

    

/// A binder for a game-state model of a user-defined type, a drawing function
/// and a frame-advance function.
type SpecificGameState<'Model>
    ( model     : 'Model, 
      drawFunc  : RenderFunction -> 'Model -> GameTime -> unit, 
      frameFunc : SpecificGameState<'Model> -> KeyStateFunction -> GameTime -> GameTime -> ErasedGameState,
      sounds    : SoundOperation list ) =

    inherit ErasedGameState()

    /// The model state for this animation frame.
    member val Model     = model

    /// The function that draws the model.
    member val DrawFunc  = drawFunc

    /// The function that calculates the next game state from this frame's model.
    member val FrameFunc = frameFunc

    /// The sound effect commands for this frame
    member val SoundEffectCommands = sounds

    override this.Draw render gameTime = 
        this.DrawFunc render this.Model gameTime

    override this.Frame keyStateGetter gameTime frameElapsedTime = 
        this.FrameFunc this keyStateGetter gameTime frameElapsedTime   // 'this.Model' is the old state

    override this.Sounds () =
        this.SoundEffectCommands


/// Returning a game state unchanged, so the model, draw handler, and 
/// frame handler remain the same.  However, SoundEffectCommands are
/// removed if present to avoid repeated re-triggering of any sound.
/// Casts to base (ErasedGameState).
let Unchanged (gameState:SpecificGameState<'Model>) =
    if gameState.SoundEffectCommands.IsEmpty then
        gameState :> ErasedGameState
    else
        (new SpecificGameState<'Model>(gameState.Model, gameState.DrawFunc, gameState.FrameFunc, []))
            :> ErasedGameState


/// Obtain the model record from the SpecificGameState.
let inline ModelFrom (gameState:SpecificGameState<'Model>) =
    gameState.Model


/// Return a new game state with a model instance, no sound commands, frame handler and drawing handler.
let inline NewGameState frameFunc drawFunc model =
    (new SpecificGameState<'Model>(model, drawFunc, frameFunc, []))
        :> ErasedGameState


/// Return a new game state with a model instance, frame handler and drawing handler, and sound commands for this frame.
let inline NewGameStateAndSounds frameFunc drawFunc model sounds =
    (new SpecificGameState<'Model>(model, drawFunc, frameFunc, sounds))
        :> ErasedGameState


/// Update the model instance, clear sound commands, but keep the model type and handler functions the same.
let inline WithUpdatedModel model (gameState:SpecificGameState<'Model>) =
    NewGameState (gameState.FrameFunc) (gameState.DrawFunc) model


/// Update the model instance, keep the model type and handler functions the same, and include sound commands.
let inline WithUpdatedModelAndSounds model sounds (gameState:SpecificGameState<'Model>) =
    NewGameStateAndSounds (gameState.FrameFunc) (gameState.DrawFunc) model sounds


/// Update the model instance, clear sound commands, but keep the model type and handler functions the same.
let inline ReplacesModelIn gameState model =
    WithUpdatedModel model gameState


/// Update the model instance and frame handler function, clear sound commands, but keep the drawing function the same.
let inline WithUpdatedModelAndFrameFunc model frameFunc (gameState:SpecificGameState<'Model>) =
    NewGameState frameFunc (gameState.DrawFunc) model


/// A FrameFunc for those screens that never change their models.
let ModelNeverChanges gameState _keyStateGetter _gameTime _elapsed =
    Unchanged gameState


/// Freeze a game state by wrapping it.   It will see no further model updates.  
/// Allow its drawing handler to see the elapsing (current) game time.
let WithoutAnyFurtherUpdates (gameState:ErasedGameState) =
    let drawFunc render _model gameTime = gameState.Draw render gameTime
    NewGameState ModelNeverChanges drawFunc ()


/// Completely freeze a game state by wrapping it.  It will see no further model updates.  
/// Its drawing handler will see the same time from now on.
let FrozenInTimeAt gameTime (gameState:ErasedGameState) =
    let drawSameFrame render _ _ = gameState.Draw render gameTime
    NewGameState ModelNeverChanges drawSameFrame ()


let WithOneShotSound oneShotSoundOperations (innerGameState:ErasedGameState) =

    let frameFunc _gameState keyStateGetter gameTime elapsed =
        // Just delegate the call, and whatever innerGameState returns 
        // will naturally replace us on the next frame:
        innerGameState.Frame keyStateGetter gameTime elapsed  

    let drawFunc render _model (gameTime:GameTime) =
        // Just delegate drawing.
        // This will only be called once because of the replacement done by frameFunc
        innerGameState.Draw render gameTime

    NewGameStateAndSounds frameFunc drawFunc () oneShotSoundOperations
