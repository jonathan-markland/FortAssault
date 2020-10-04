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

            // Global data that never changes at all, even across game-starts:

            let fortAssaultStaticData = 
                { TankMapsList = tankMapsList }

            // Global data that is survives gameplay sessions:

            let fortAssaultGlobals = 
                InitialFortAssaultGlobals ()

            // Constructor function that starts a new game:

            let fortAssaultGameplayStartConstructor = 
                NewStoryboard fortAssaultStaticData

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
                FortAssaultResourceImages 
                FortAssaultFontResourceImages
                fortAssaultKeysNeeded
                fortAssaultStaticData
                fortAssaultGlobals
                fortAssaultGameplayStartConstructor
                RenderStoryboard
                NextStoryboardState

        | Error msg -> 
            System.Console.WriteLine(msg) |> ignore
            0


