module PacmanMainModule

open SDL2  // TODO: It would be nicer if SDLCover could provide everything.

open PacmanImageFiles
open Input
open DesktopGameFramework
open Storyboard


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
            (SDL.SDL_Scancode.SDL_SCANCODE_LEFT  , WebBrowserKeyCode 37)
            (SDL.SDL_Scancode.SDL_SCANCODE_RIGHT , WebBrowserKeyCode 39)
            (SDL.SDL_Scancode.SDL_SCANCODE_UP    , WebBrowserKeyCode 38)
            (SDL.SDL_Scancode.SDL_SCANCODE_DOWN  , WebBrowserKeyCode 40)
            (SDL.SDL_Scancode.SDL_SCANCODE_Z     , WebBrowserKeyCode 90)
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
