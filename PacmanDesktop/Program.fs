module PacmanMainModule

open SDL2  // TODO: It would be nicer if SDLCover could provide everything.

open PacmanImageFiles
open Input
open DesktopGameFramework
open Storyboard
open Keys


let PacmanRetroScreenWidthPixels  =  320 
let PacmanRetroScreenHeightPixels =  256
let PacmanWindowWidthPixels       = PacmanRetroScreenWidthPixels  * 3 
let PacmanWindowHeightPixels      = PacmanRetroScreenHeightPixels * 3


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
            "Pacman"
            PacmanWindowWidthPixels
            PacmanWindowHeightPixels
            PacmanRetroScreenWidthPixels
            PacmanRetroScreenHeightPixels
            PacmanResourceImages 
            PacmanFontResourceImages
            keysNeeded
            stubGlobalStateConstructor
            NewPacmanStoryboard
            with

        | Some errorMessage ->
            printfn "%s" errorMessage
            ProcessExitFail

        | None ->
            ProcessExitSuccess
