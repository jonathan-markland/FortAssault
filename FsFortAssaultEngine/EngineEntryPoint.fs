module EngineEntryPoint

open Storyboard
open ScreenHandler



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
    model |> NewGameState NextStoryboardState RenderFortAssaultStoryboard




