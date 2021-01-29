module MissionIIMainModule

open SDL2  // TODO: It would be nicer if SDLCover could provide everything.

open MissionIIResourceFiles
open Input
open DesktopGameFramework
open Storyboard
open Keys


let MissionIIRetroScreenWidthPixels  =  320 
let MissionIIRetroScreenHeightPixels =  256
let MissionIIWindowWidthPixels       = MissionIIRetroScreenWidthPixels  * 2 
let MissionIIWindowHeightPixels      = MissionIIRetroScreenHeightPixels * 2


let ProcessExitFail    = 1
let ProcessExitSuccess = 0


[<EntryPoint>]
let main argv =

    let keysNeeded =
        [
            (SDL.SDL_Scancode.SDL_SCANCODE_LEFT  , KeyLeft)
            (SDL.SDL_Scancode.SDL_SCANCODE_RIGHT , KeyRight)
            (SDL.SDL_Scancode.SDL_SCANCODE_UP    , KeyUp)
            (SDL.SDL_Scancode.SDL_SCANCODE_DOWN  , KeyDown)
            (SDL.SDL_Scancode.SDL_SCANCODE_Z     , KeyFire)
        ]

    let stubGlobalStateConstructor () = Ok ()  // Feature not used

    match FrameworkDesktopMain 
            "MissionII"
            MissionIIWindowWidthPixels
            MissionIIWindowHeightPixels
            MissionIIRetroScreenWidthPixels
            MissionIIRetroScreenHeightPixels
            MissionIIResourceImages 
            MissionIIFontResourceImages
            MissionIIResourceSounds
            keysNeeded
            stubGlobalStateConstructor
            NewMissionIIStoryboard
            with

        | Some errorMessage ->
            printfn "%s" errorMessage
            ProcessExitFail

        | None ->
            ProcessExitSuccess
