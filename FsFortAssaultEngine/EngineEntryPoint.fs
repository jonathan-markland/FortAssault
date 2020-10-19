module EngineEntryPoint

open Input
open InputEventData
open Storyboard
open ScreenHandler



let NextFortAssaultStoryboardState staticGameResources gameState keyStateGetter gameTime frameElapsedTime =

    // TODO: When I refactor away the storyboard DU, the gameState will be passed through "neat"
    //       rather than passing the model from the gameState:
    let nextModel = NextStoryboardState staticGameResources (ModelFrom gameState) keyStateGetter gameTime frameElapsedTime

    gameState |> WithUpdatedModel nextModel


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//   Creation of initial SpecificGameState
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Called only once when the game boots
let NewFortAssaultStoryboard gameResources gameGlobalState gameTime =

    #if SHORT_PLAYTHROUGH
    let storyboard =
        Shortcut gameResources gameTime SkipToEnterYourName // RunGameNormally
    #else
    let storyboard =
        Shortcut gameResources gameTime RunGameNormally  // ** DO NOT CHANGE THIS ONE! : Define SHORT_PLAYTHROUGH and set the one above **
    #endif

    let model = struct (storyboard , gameGlobalState)
    model |> NewGameState NextFortAssaultStoryboardState RenderFortAssaultStoryboard




