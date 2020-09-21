module DesktopMain

open SDL2  // TODO: It would be nicer if SDLCover could provide everything.
open System.IO
open SDLCover
open Time
open Geometry
open DrawingCommands
open GameGlobalState
open KeyboardForFramework
open Storyboard
open EngineEntryPoint
open TankMapFileLoader
open ResourceFileMetadata
open ResourceFiles
open StaticResourceSetup
open Input

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let HostWindowWidthPixels = 1280 
let HostWindowHeightPixels = 800
    
let HostRetroScreenWidthPixels = 320 
let HostRetroScreenHeightPixels = 200

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type GameResourcesRecord =
    {
        GameBMPs    : ImageWithHostObject[]
        Fonts       : NumCapsFontDefinition[]
    }


let LoadGameImagesAndFonts (renderer:RendererNativeInt) rootPath =

    let fromFile transparencyColour name = 

        let fullPath =
            Path.Combine(Path.Combine(rootPath, "Images"), name)
        
        match LoadFromFileAndPrepareForRenderer renderer fullPath transparencyColour with
            | Some(imageRecord) -> imageRecord
            | None -> failwith (sprintf "Game could not start because file '%s' is missing or has invalid content." fullPath)

    let unwrapFont opt =
        match opt with
            | Some(font) -> font
            | None -> failwith "Game could not start because a font file has incorrect content."

    let magenta =
        Some({ Red=255uy ; Green=0uy ; Blue=255uy })
    
    let imagesArray =
        GameResourceImages 
            |> List.map (fun metadata -> 
                
                let key = 
                    match metadata.ImageColourKey with 
                        | NoColourKey -> None 
                        | MagentaColourKey -> magenta
                
                let fileName = metadata.ImageFileName
                
                let hostImageObject = fromFile key fileName

                {
                    EngineImageMetadata = metadata
                    HostImageObject     = HostImageObject(hostImageObject)
                })

            |> List.toArray

    let fontsArray =
        GameFontResourceImages 
            |> List.map (fun metadata -> 
                fromFile magenta metadata.ImageFileName
                    |> MakeNumCapsFontFromBMP 
                    |> unwrapFont) 
                        |> List.toArray

    {
        GameBMPs = imagesArray
        Fonts    = fontsArray
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Render game drawing command to the screen.
/// Here we are choosing to use a 1:1 mapping from engine 'epx' coordinates onto a 
/// HostRetroScreenWidthPixels x HostRetroScreenHeightPixels pixel SDL surface, but 
/// using Float32 allowing higher resolution positioning if this side supported it(!)
let RenderToSdl gameResources renderer drawingCommand =

    /// Convert engine units to our pixels.
    /// Currently this host is choosing to use 1:1 with the engine's coordinate scheme.
    let px (n:float32<epx>) =
        FloatEpxToInt n

    let numCapsFontImageDefinitionFor (FontID(fontIndex)) =
        let fontSet = gameResources.Fonts
        if fontIndex >= 0 && fontIndex < fontSet.Length then
            fontSet.[fontIndex]
        else
            failwith "invalid font resource index"

    match drawingCommand with

        | DrawImageWithTopLeftAtInt(left, top, imageVisual) ->
            let (HostImageObject(hostImageObject)) = imageVisual.HostImageObject
            DrawImage 
                renderer 
                (hostImageObject :?> ImageFileMetadata)
                ((int) left) 
                ((int) top) // NB: not truncations, just removing the units of measure

        | DrawStretchedImageWithTopLeftAt(left, top, imageVisual, width, height) ->
            let (HostImageObject(hostImageObject)) = imageVisual.HostImageObject
            DrawSubImage 
                renderer 
                (hostImageObject :?> ImageFileMetadata).TextureHandle
                0 0 
                ((int) imageVisual.EngineImageMetadata.ImageWidth)
                ((int) imageVisual.EngineImageMetadata.ImageHeight)
                (px left) (px top) (px width) (px height)

        | DrawSubImageStretchedToTarget(srcleft, srctop, srcwidth, srcheight, dstleft, dsttop, dstwidth, dstheight, imageVisual) ->
            let (HostImageObject(hostImageObject)) = imageVisual.HostImageObject
            DrawSubImage 
                renderer 
                (hostImageObject :?> ImageFileMetadata).TextureHandle
                srcleft srctop srcwidth srcheight
                (px dstleft) (px dsttop) (px dstwidth) (px dstheight)

        | DrawCharImageWithTopLeftAt(left, top, charIndex, fontVisual) ->
            let fontDefinition = numCapsFontImageDefinitionFor fontVisual
            let cwid = fontDefinition.CharWidth
            let chei = fontDefinition.CharHeight
            let chx  = (int charIndex) * cwid // TODO: constant: assuming char with for fonts.
            DrawSubImage 
                renderer 
                fontDefinition.FontImage.TextureHandle
                chx 0 cwid chei 
                (left |> IntEpxToInt) (top |> IntEpxToInt) cwid chei

        | DrawFilledRectangle(left, top, width, height, SolidColour(colour)) ->
            let right  = (left + width) |> IntEpxToInt
            let bottom = (top + height) |> IntEpxToInt
            let left   = left |> IntEpxToInt
            let top    = top  |> IntEpxToInt
            SDLCover.DrawFilledRectangle renderer left top right bottom colour


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let TimerCallback (interval:uint32) (param:nativeint) : uint32 =  // TODO: Can this go into the SDL library?

    let mutable event = new SDL.SDL_Event()

    event.``type`` <- SDL.SDL_EventType.SDL_USEREVENT
    event.user.code <- 0
    event.user.data1 <- 0n
    event.user.data2 <- 0n

    SDL.SDL_PushEvent(&event) |> ignore
    interval  // We can return 0u to cancel the timer here, or interval to keep it going.



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let MainLoopProcessing renderer backingTexture tankMapsList gameResources =

    SetStaticImageResourceArray gameResources.GameBMPs

    let mutable tickCount = 1u
    
    let GetGameTime () = 
        LanguagePrimitives.Float32WithMeasure<seconds> ((float32 tickCount) / 50.0F)

    let staticGameResources = 
        {
            TankMapsList = tankMapsList
        }

    let initGameGlobals = InitialGameGlobals ()
    let initScreenState = NewStoryboard staticGameResources (GetGameTime ()) // TODO: Ideally don't pass gameResources -- only needed for hacking access to a particular screen.
    let mutable screenState = (struct (initScreenState , initGameGlobals))

    // 20ms timer installed so that the main event loop receives 'SDL.SDL_EventType.SDL_USEREVENT' every 20ms (1/50th second)
    let timerID =   // TODO: Push into library?
        SDL.SDL_AddTimer(20u, new SDL.SDL_TimerCallback(TimerCallback), 0n)
            
    if timerID = 0 then
        failwith "Failed to install the gameplay timer."

    let renderFunction = 
        RenderToSdl gameResources renderer   // TODO: We only pass gameResources in to get the fonts now.  Soon we won't need to pass gameResources at all.


    let mutableKeyStateStore =
        NewMutableKeyStateStore
            SDL.SDL_Scancode.SDL_SCANCODE_P
            [
                (SDL.SDL_Scancode.SDL_SCANCODE_LEFT  , WebBrowserKeyCode 37)
                (SDL.SDL_Scancode.SDL_SCANCODE_RIGHT , WebBrowserKeyCode 39)
                (SDL.SDL_Scancode.SDL_SCANCODE_UP    , WebBrowserKeyCode 38)
                (SDL.SDL_Scancode.SDL_SCANCODE_DOWN  , WebBrowserKeyCode 40)
                (SDL.SDL_Scancode.SDL_SCANCODE_Z     , WebBrowserKeyCode 90)
            ]
    
    let keyStateGetter = LiveKeyStateFrom mutableKeyStateStore

    let HandleFrameAdvanceEvent gameTime lastGameTime =

        SetRenderTargetToTexture renderer backingTexture

        // DEBUG: force clean the drawing texture.  This may help observe artefacts where tiles don't join.
        // renderFunction (DrawFilledRectangle(0.0F<wu>, 0.0F<wu>, 320.0F<wu>, 256.0F<wu>, SolidColour(0xFF00FFu)))

        RenderStoryboard renderFunction screenState gameTime
        SetRenderTargetToScreen renderer
        RenderCopyToFullTarget renderer backingTexture
        Present renderer

        let frameElapsedTime =
            gameTime - lastGameTime

        let nextScreenState = 
            NextGameState 
                staticGameResources 
                screenState 
                keyStateGetter
                gameTime 
                frameElapsedTime

        tickCount <- tickCount + 1u
        screenState <- nextScreenState

        mutableKeyStateStore |> ClearKeyJustPressedFlags


    let mutable event            = new SDL.SDL_Event ()
    let mutable lastGameTime     = 0.0F<seconds>
    let mutable terminateProgram = false

    while terminateProgram = false do
        while (SDL.SDL_WaitEvent (&event)) <> 0 && not terminateProgram do

            let msg = event.``type``

            if msg = SDL.SDL_EventType.SDL_QUIT then 
                terminateProgram <- true

            else if msg = SDL.SDL_EventType.SDL_KEYDOWN then
                let code = event.key.keysym.scancode
                HandleKeyDownEvent mutableKeyStateStore code |> ignore

            else if msg = SDL.SDL_EventType.SDL_KEYUP then
                let code = event.key.keysym.scancode
                HandleKeyUpEvent mutableKeyStateStore code |> ignore

            else if msg = SDL.SDL_EventType.SDL_USEREVENT then
                let gameTime = GetGameTime ()
                if not (mutableKeyStateStore |> IsGamePaused) then
                    HandleFrameAdvanceEvent gameTime lastGameTime
                lastGameTime <- gameTime



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let GameMain () =

    match LoadTankBattleSequences () with
        
        | Ok tankMapsList ->
            match CreateWindowAndRenderer "Fort Assault" HostWindowWidthPixels HostWindowHeightPixels with   // TODO: Re-visit window initial size constants
                
                | Some(_mainWindow, renderer) ->
                    match CreateRgb8888TextureForRenderer renderer HostRetroScreenWidthPixels HostRetroScreenHeightPixels with

                        | Some(backingTexture) ->
                            let gameResources = LoadGameImagesAndFonts renderer ""   // TODO:  Minor: We don't actually free the imageSet handles.
                            MainLoopProcessing renderer backingTexture tankMapsList gameResources
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

    match WithSdl2Do GameMain with

        | None -> 
            printfn "Failed to start SDL2 library."   // TODO: Let's not use the STDOUT.
            0

        | Some(n) -> 
            n
