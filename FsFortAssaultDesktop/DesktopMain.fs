module DesktopMain

open SDL2  // TODO: It would be nicer if SDLCover could provide everything.
open System.IO
open SDLCover
open GameGlobalState
open Storyboard
open EngineEntryPoint
open TankMapFileLoader
open ResourceFileMetadata
open ResourceFiles
open Input
open DesktopGameFramework

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let HostWindowWidthPixels = 1280 
let HostWindowHeightPixels = 800
    
let HostRetroScreenWidthPixels = 320 
let HostRetroScreenHeightPixels = 200

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let GameMain gameWindowTitleString () =

    match LoadTankBattleSequences () with // TODO: These are static resources now.
        
        | Ok tankMapsList ->
            match CreateWindowAndRenderer gameWindowTitleString HostWindowWidthPixels HostWindowHeightPixels with   // TODO: Re-visit window initial size constants
                
                | Some(_mainWindow, renderer) ->
                    match CreateRgb8888TextureForRenderer renderer HostRetroScreenWidthPixels HostRetroScreenHeightPixels with

                        | Some(backingTexture) ->
                            let path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
                            let gameResources = LoadGameImagesAndFonts GameResourceImages GameFontResourceImages renderer path   // TODO:  Minor: We don't actually free the imageSet handles.

                            let staticGameResources = 
                                {
                                    TankMapsList = tankMapsList
                                }

                            let initialGameStateConstructor = 
                                NewStoryboard staticGameResources

                            let initGameGlobals = InitialGameGlobals ()

                            MainLoopProcessing 
                                renderer 
                                backingTexture 
                                gameResources 
                                staticGameResources 
                                initialGameStateConstructor
                                initGameGlobals
                                RenderStoryboard 
                                NextGameState
                                [
                                    (SDL.SDL_Scancode.SDL_SCANCODE_LEFT  , WebBrowserKeyCode 37)
                                    (SDL.SDL_Scancode.SDL_SCANCODE_RIGHT , WebBrowserKeyCode 39)
                                    (SDL.SDL_Scancode.SDL_SCANCODE_UP    , WebBrowserKeyCode 38)
                                    (SDL.SDL_Scancode.SDL_SCANCODE_DOWN  , WebBrowserKeyCode 40)
                                    (SDL.SDL_Scancode.SDL_SCANCODE_Z     , WebBrowserKeyCode 90)
                                ]


                            1
            
                        | None ->
                            failwith "Cannot create backing texture."

                | None ->
                    0

        | Error msg -> 
            System.Console.WriteLine(msg) |> ignore
            0



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let DesktopMain () =

    match WithSdl2Do (GameMain "Fort Assault") with

        | None -> 
            printfn "Failed to start SDL2 library."   // TODO: Let's not use the STDOUT.
            0

        | Some(n) -> 
            n
