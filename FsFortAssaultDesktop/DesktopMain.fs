module DesktopMain

open SDL2  // TODO: It would be nicer if SDLCover could provide everything.
open SDLCover
open FortAssaultGlobalState
open Storyboard
open EngineEntryPoint
open TankMapFileLoader
open ResourceFiles
open Input
open DesktopGameFramework

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let GameMain
    gameWindowTitleString 
    hostWindowWidthPixels 
    hostWindowHeightPixels 
    hostRetroScreenWidthPixels 
    hostRetroScreenHeightPixels 
    keysListNeeded 
    staticGameResources
    gameResourceImages 
    gameFontResourceImages
    initialGameStateConstructor
    gameGlobals
    () =

        match CreateWindowAndRenderer gameWindowTitleString hostWindowWidthPixels hostWindowHeightPixels with   // TODO: Re-visit window initial size constants
                
            | Some(_mainWindow, renderer) ->

                match CreateRgb8888TextureForRenderer renderer hostRetroScreenWidthPixels hostRetroScreenHeightPixels with

                    | Some(backingTexture) ->

                        let path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
                        let gameResources = LoadGameImagesAndFonts gameResourceImages gameFontResourceImages renderer path   // TODO:  Minor: We don't actually free the imageSet handles.

                        MainLoopProcessing 
                            renderer 
                            backingTexture 
                            gameResources 
                            staticGameResources 
                            initialGameStateConstructor
                            gameGlobals
                            RenderStoryboard 
                            NextStoryboardState
                            keysListNeeded

                        1
            
                    | None ->
                        failwith "Cannot create an SDL2 texture to store the game screen image."

            | None ->
                0




// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DesktopMain2 
    gameName 
    hostWindowWidthPixels
    hostWindowHeightPixels
    hostRetroScreenWidthPixels
    hostRetroScreenHeightPixels
    keysListNeeded 
    staticGameResources 
    gameResourceImages 
    gameFontResourceImages 
    initialGameStateConstructor
    gameGlobals =

    match WithSdl2Do (GameMain gameName hostWindowWidthPixels hostWindowHeightPixels hostRetroScreenWidthPixels hostRetroScreenHeightPixels keysListNeeded staticGameResources gameResourceImages gameFontResourceImages initialGameStateConstructor gameGlobals) with

        | None -> 
            printfn "Cannot start the game because the SDL2 library failed to start."
            0

        | Some(n) -> 
            n





// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//   vvv Fort-Assault specific stuff vvv             ^^^ Framework for any game ^^^
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let HostWindowWidthPixels = 1280 
let HostWindowHeightPixels = 800
    
let HostRetroScreenWidthPixels = 320 
let HostRetroScreenHeightPixels = 200


let DesktopMain () =

    match LoadTankBattleSequences () with // TODO: These are static resources now.
    
        | Ok tankMapsList ->

            let staticGameResources = 
                {
                    TankMapsList = tankMapsList
                }

            let initialGameStateConstructor = 
                NewStoryboard staticGameResources

            let fortAssaultGlobals = InitialFortAssaultGlobals ()

            DesktopMain2 
                "Fort Assault"
                HostWindowWidthPixels
                HostWindowHeightPixels
                HostRetroScreenWidthPixels
                HostRetroScreenHeightPixels
                [
                    (SDL.SDL_Scancode.SDL_SCANCODE_LEFT  , WebBrowserKeyCode 37)
                    (SDL.SDL_Scancode.SDL_SCANCODE_RIGHT , WebBrowserKeyCode 39)
                    (SDL.SDL_Scancode.SDL_SCANCODE_UP    , WebBrowserKeyCode 38)
                    (SDL.SDL_Scancode.SDL_SCANCODE_DOWN  , WebBrowserKeyCode 40)
                    (SDL.SDL_Scancode.SDL_SCANCODE_Z     , WebBrowserKeyCode 90)
                ]
                staticGameResources
                GameResourceImages 
                GameFontResourceImages
                initialGameStateConstructor
                fortAssaultGlobals

        | Error msg -> 
            System.Console.WriteLine(msg) |> ignore
            0


