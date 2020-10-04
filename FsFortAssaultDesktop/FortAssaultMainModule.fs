module FortAssaultMainModule

open SDL2  // TODO: It would be nicer if SDLCover could provide everything.

open ResourceFiles
open Input
open DesktopGameFramework
open FortAssaultGlobalState
open Storyboard
open TankMapFileLoader
open EngineEntryPoint


let FortAssaultWindowWidthPixels       = 1280 
let FortAssaultWindowHeightPixels      =  800
let FortAssaultRetroScreenWidthPixels  =  320 
let FortAssaultRetroScreenHeightPixels =  200



let FortAssaultMain () =

    match LoadTankBattleSequences () with // TODO: These are static resources now.
    
        | Ok tankMapsList ->

            let fortAssaultStaticResources = 
                { TankMapsList = tankMapsList }

            let fortAssaultGlobals = 
                InitialFortAssaultGlobals ()

            let fortAssaultStartStateConstructor = 
                NewStoryboard fortAssaultStaticResources

            let fortAssaultKeysNeeded =
                [
                    (SDL.SDL_Scancode.SDL_SCANCODE_LEFT  , WebBrowserKeyCode 37)
                    (SDL.SDL_Scancode.SDL_SCANCODE_RIGHT , WebBrowserKeyCode 39)
                    (SDL.SDL_Scancode.SDL_SCANCODE_UP    , WebBrowserKeyCode 38)
                    (SDL.SDL_Scancode.SDL_SCANCODE_DOWN  , WebBrowserKeyCode 40)
                    (SDL.SDL_Scancode.SDL_SCANCODE_Z     , WebBrowserKeyCode 90)
                ]

            FrameworkDesktopMain 
                "Fort Assault"
                FortAssaultWindowWidthPixels
                FortAssaultWindowHeightPixels
                FortAssaultRetroScreenWidthPixels
                FortAssaultRetroScreenHeightPixels
                fortAssaultKeysNeeded
                fortAssaultStaticResources
                GameResourceImages 
                GameFontResourceImages
                fortAssaultStartStateConstructor
                RenderStoryboard
                NextStoryboardState
                fortAssaultGlobals

        | Error msg -> 
            System.Console.WriteLine(msg) |> ignore
            0


