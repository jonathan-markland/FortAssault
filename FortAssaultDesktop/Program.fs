module FortAssaultMainModule

open SDL2  // TODO: It would be nicer if SDLCover could provide everything.

open FortAssaultResourceFiles
open Input
open DesktopGameFramework
open Storyboard
open MechanicsTestPage


let FortAssaultWindowWidthPixels       = 1280 
let FortAssaultWindowHeightPixels      =  800
let FortAssaultRetroScreenWidthPixels  =  320 
let FortAssaultRetroScreenHeightPixels =  200


let ProcessExitFail    = 1
let ProcessExitSuccess = 0


[<EntryPoint>]
let main argv =

    let fortAssaultKeysNeeded =
        [
            (SDL.SDL_Scancode.SDL_SCANCODE_LEFT  , WebBrowserKeyCode 37)
            (SDL.SDL_Scancode.SDL_SCANCODE_RIGHT , WebBrowserKeyCode 39)
            (SDL.SDL_Scancode.SDL_SCANCODE_UP    , WebBrowserKeyCode 38)
            (SDL.SDL_Scancode.SDL_SCANCODE_DOWN  , WebBrowserKeyCode 40)
            (SDL.SDL_Scancode.SDL_SCANCODE_Z     , WebBrowserKeyCode 90)
        ]

    let stubGlobalStateConstructor () = Ok ()  // Feature not used by Fort Assault.

    match FrameworkDesktopMain 
            "Fort Assault"
            FortAssaultWindowWidthPixels
            FortAssaultWindowHeightPixels
            FortAssaultRetroScreenWidthPixels
            FortAssaultRetroScreenHeightPixels
            FortAssaultResourceImages 
            FortAssaultFontResourceImages
            fortAssaultKeysNeeded
            stubGlobalStateConstructor
            NewFortAssaultStoryboard  // Or, swap out for NewMechanicsTestPageStoryboard
            with

        | Some errorMessage ->
            printfn "%s" errorMessage
            ProcessExitFail

        | None ->
            ProcessExitSuccess
