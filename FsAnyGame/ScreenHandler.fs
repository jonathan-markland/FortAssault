module ScreenHandler

open Time
open DrawingShapes
open Input


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
    abstract member Draw  : RenderFunction -> float32<seconds> -> unit

    /// Calculate the model value, Draw function and Frame function for the next 
    /// frame of animation.
    abstract member Frame : KeyStateFunction -> float32<seconds> -> float32<seconds> -> ErasedGameState


/// A binder for a game-state model of a user-defined type, a drawing function
/// and a frame-advance function.
type SpecificGameState<'Model>
    ( model     : 'Model, 
      drawFunc  : RenderFunction -> 'Model -> float32<seconds> -> unit, 
      frameFunc : SpecificGameState<'Model> -> KeyStateFunction -> float32<seconds> -> float32<seconds> -> ErasedGameState ) =

    inherit ErasedGameState()

    /// The model state for this animation frame.
    member val Model     = model

    /// The function that draws the model.
    member val DrawFunc  = drawFunc

    /// The function that calculates the next game state from this frame's model.
    member val FrameFunc = frameFunc

    override this.Draw render gameTime = 
        this.DrawFunc render this.Model gameTime

    override this.Frame keyStateGetter gameTime frameElapsedTime = 
        this.FrameFunc this keyStateGetter gameTime frameElapsedTime   // 'this.Model' is the old state


/// Returning a game state unchanged.  Casts to base.
let inline Unchanged (gameState:SpecificGameState<'Model>) =
    gameState :> ErasedGameState


/// Obtain the model record from the SpecificGameState.
let inline ModelFrom (gameState:SpecificGameState<'Model>) =
    gameState.Model


/// Return a new game state with a model instance, frame handler and drawing handler.
let inline NewGameState frameFunc drawFunc model =
    (new SpecificGameState<'Model>(model, drawFunc, frameFunc))
        :> ErasedGameState


/// Update the model instance, but keep the model type and handler functions the same.
let inline WithUpdatedModel model (gameState:SpecificGameState<'Model>) =
    NewGameState (gameState.FrameFunc) (gameState.DrawFunc) model

/// Update the model instance, but keep the model type and handler functions the same.
let inline ReplacesModelIn gameState model =
    WithUpdatedModel model gameState


/// Update the model instance and frame handler function, but keep the drawing function the same.
let inline WithUpdatedModelAndFrameFunc model frameFunc (gameState:SpecificGameState<'Model>) =
    NewGameState frameFunc (gameState.DrawFunc) model

