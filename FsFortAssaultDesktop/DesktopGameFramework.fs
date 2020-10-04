/// Framework for SDL2-Desktop games.
module DesktopGameFramework

open System.IO
open SDL2  // TODO: It would be nicer if SDLCover could provide everything.
open SDLCover

open StaticResourceSetup
open KeyboardForFramework

open Time
open Geometry
open DrawingCommands
open ResourceFileMetadata

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type FrameworkGameResourcesRecord =
    {
        GameBMPs    : ImageWithHostObject[]
        Fonts       : NumCapsFontDefinition[]
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let LoadGameImagesAndFonts gameResourceImages gameFontResourceImages (renderer:RendererNativeInt) rootPath =  // TODO: Result error string

    let fromFile transparencyColour name = 

        let fullPath =
            Path.Combine(rootPath, name)
        
        if not (File.Exists(fullPath)) then
            failwith (sprintf "Game could not start because file '%s' is missing." fullPath)

        match LoadFromFileAndPrepareForRenderer renderer fullPath transparencyColour with
            | Some(imageRecord) -> imageRecord
            | None -> failwith (sprintf "Game could not start because file '%s' has invalid content." fullPath)

    let unwrapFont fileName opt =
        match opt with
            | Some(font) -> font
            | None -> failwith (sprintf "Game could not start because font '%s' file has incorrect content." fileName)

    let magenta =
        Some({ Red=255uy ; Green=0uy ; Blue=255uy })
    
    let imagesArray =
        gameResourceImages
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
        gameFontResourceImages 
            |> List.map (fun metadata -> 
                fromFile magenta metadata.ImageFileName
                    |> MakeNumCapsFontFromBMP 
                    |> unwrapFont metadata.ImageFileName) 
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

let MainLoopProcessing 
    renderer 
    backingTexture 
    gameResources 
    gameStaticData 
    initialGameStateConstructor 
    initGameGlobals 
    gameRenderer 
    gameFrameAdvanceFunction 
    listOfKeysNeeded =

    SetStaticImageResourceArray gameResources.GameBMPs

    let mutable tickCount = 1u
    
    let GetGameTime () = 
        (float32 tickCount) / 50.0F |> InSeconds

    let initScreenState = 
        initialGameStateConstructor (GetGameTime ())

    let mutable screenState = 
        (struct (initScreenState , initGameGlobals))

    // 20ms timer installed so that the main event loop receives 'SDL.SDL_EventType.SDL_USEREVENT' every 20ms (1/50th second)
    let timerID =
        SDL.SDL_AddTimer(20u, new SDL.SDL_TimerCallback(TimerCallback), 0n)
            
    if timerID = 0 then
        failwith "Failed to install the gameplay timer."

    let renderFunction = 
        RenderToSdl gameResources renderer   // TODO: We only pass gameResources in to get the fonts now.  Soon we won't need to pass gameResources at all.

    let mutableKeyStateStore =
        NewMutableKeyStateStore
            SDL.SDL_Scancode.SDL_SCANCODE_P
            listOfKeysNeeded
    
    let keyStateGetter = 
        LiveKeyStateFrom mutableKeyStateStore

    let HandleFrameAdvanceEvent gameTime lastGameTime =

        SetRenderTargetToTexture renderer backingTexture

        // DEBUG: force clean the drawing texture.  This may help observe artefacts where tiles don't join.
        // renderFunction (DrawFilledRectangle(0.0F<wu>, 0.0F<wu>, 320.0F<wu>, 256.0F<wu>, SolidColour(0xFF00FFu)))

        gameRenderer renderFunction screenState gameTime
        SetRenderTargetToScreen renderer
        RenderCopyToFullTarget renderer backingTexture
        Present renderer

        let frameElapsedTime =
            gameTime - lastGameTime  // TODO: Why calculate this.  Web version just passes constant.

        let nextScreenState = 
            gameFrameAdvanceFunction 
                gameStaticData 
                screenState 
                keyStateGetter
                gameTime 
                frameElapsedTime  // TODO: Didn't like passing this really.

        tickCount <- tickCount + 1u
        screenState <- nextScreenState

        mutableKeyStateStore |> ClearKeyJustPressedFlags


    // Classic main event loop.

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

let FrameworkDesktopMain
    gameWindowTitleString 
    hostWindowWidthPixels 
    hostWindowHeightPixels 
    hostRetroScreenWidthPixels 
    hostRetroScreenHeightPixels 
    gameResourceImages 
    gameFontResourceImages
    listOfKeysNeeded 
    (gameStaticDataConstructor  : unit -> Result<'gameStaticData,string>)
    (gameGlobalStateConstructor : unit -> Result<'gameGlobalState,string>)
    (gameplayStartConstructor   : 'gameStaticData -> float32<seconds> -> 'gameScreenModel)
    gameRenderer 
    gameFrameAdvanceFunction : string option =

        let runGame () =

            match CreateWindowAndRenderer gameWindowTitleString hostWindowWidthPixels hostWindowHeightPixels with   // TODO: Re-visit window initial size constants

                | None ->
                    Some "Main window and SDL2 renderer could not be created."
        
                | Some(_mainWindow, renderer) ->

                    match CreateRgb8888TextureForRenderer renderer hostRetroScreenWidthPixels hostRetroScreenHeightPixels with

                        | None ->
                            Some "Cannot create an SDL2 texture to store the game screen image."

                        | Some(backingTexture) ->

                            let path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
                            
                            let gameResources = 
                                LoadGameImagesAndFonts gameResourceImages gameFontResourceImages renderer path   // TODO:  Minor: We don't actually free the imageSet handles.

                            let gameStaticDataResult =
                                gameStaticDataConstructor ()

                            let gameGlobalStateResult = 
                                gameGlobalStateConstructor ()

                            let errorResultToOption result =
                                match result with
                                    | Ok _ -> None
                                    | Error errorMessage -> Some errorMessage

                            gameGlobalStateResult
                                |> Result.map (fun gameGlobalState ->

                                    gameStaticDataResult
                                        |> Result.map (fun gameStaticData ->

                                            MainLoopProcessing 
                                                renderer 
                                                backingTexture 
                                                gameResources 
                                                gameStaticData 
                                                (gameplayStartConstructor gameStaticData)
                                                gameGlobalState
                                                gameRenderer 
                                                gameFrameAdvanceFunction
                                                listOfKeysNeeded

                                            None
                                        )
                                        |> errorResultToOption
                                )
                                |> errorResultToOption

        try

            match WithSdl2Do runGame with

                | None -> 
                    Some "The SDL2 library failed to start."

                | Some status -> 
                    status

        with 
            | e ->
                Some (sprintf "%s" (e.ToString()))
                


            

