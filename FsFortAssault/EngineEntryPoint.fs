module EngineEntryPoint

open GameGlobalState
open Input
open InputEventData
open Storyboard

let NextGameState staticGameResources (gameGlobals:GameGlobalState) storyboard keyStateGetter gameTime frameElapsedTime =

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

    NextStoryboardState staticGameResources gameGlobals storyboard input gameTime frameElapsedTime

