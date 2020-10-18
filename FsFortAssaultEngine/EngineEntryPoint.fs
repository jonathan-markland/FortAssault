module EngineEntryPoint

open Input
open InputEventData
open Storyboard
open ScreenHandler



let NextFortAssaultStoryboardState staticGameResources gameState keyStateGetter gameTime frameElapsedTime =

    // TODO: The following is under reconsideration, so we would pass the keyStateGetter function along:
    // We wish to disguise the keyStateGetter by NOT passing it along.
    // Let's always use browser keys in the game engine, and the host can re-map them as needed.
    // https://keycode.info/

    let u = keyStateGetter (WebBrowserKeyCode 38)
    let d = keyStateGetter (WebBrowserKeyCode 40)
    let l = keyStateGetter (WebBrowserKeyCode 37)
    let r = keyStateGetter (WebBrowserKeyCode 39)
    let f = keyStateGetter (WebBrowserKeyCode 90)

    let input =
        {
            Left  = l
            Right = r
            Up    = u
            Down  = d
            Fire  = f
        }

    // TODO: When I refactor away the storyboard DU, the gameState will be passed through "neat"
    //       rather than passing the model from the gameState:
    let nextModel = NextStoryboardState staticGameResources (ModelFrom gameState) input gameTime frameElapsedTime

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




