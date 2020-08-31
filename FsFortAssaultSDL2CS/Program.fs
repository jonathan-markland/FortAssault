open SDL2  // TODO: It would be nicer if SDLCover could provide everything.
open System.IO
open SDLCover
open Input
open InputEventData
open Time
open Geometry
open DrawingCommands
open GameGlobalState
open Keyboard
open Storyboard
open StoryboardChapters
open TankMapFileLoader
open ResourceFileMetadata
open ResourceFiles

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let HostWindowWidthPixels = 1280 
let HostWindowHeightPixels = 800
    
let HostRetroScreenWidthPixels = 320 
let HostRetroScreenHeightPixels = 200

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type GameResourcesRecord =
    {
        GameBMPs    : ImageFileMetadata[]
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
        GameResourceImages |> Array.map (fun (key, fileName) -> 
            let key = match key with NoColourKey -> None | MagentaColourKey -> magenta
            fromFile key fileName)

    let fontsArray =
        GameFontResourceImages 
            |> List.map (fun fileName -> 
                fromFile magenta fileName
                    |> MakeNumCapsFontFromBMP 
                    |> unwrapFont) 
                        |> List.toArray

    {
        GameBMPs = imagesArray
        Fonts    = fontsArray
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Render game drawing command to the screen.
/// Here we are choosing to use a 1:1 mapping from world coordinates onto a 
/// HostRetroScreenWidthPixels x HostRetroScreenHeightPixels pixel SDL surface, but 
/// using Float32 allowing higher resolution positioning if this side supported it(!)
let RenderToSdl gameResources renderer drawingCommand =

    /// Convert engine units to our pixels.
    /// Currently this host is choosing to use 1:1 with the engine's coordinate scheme.
    let px (n:float32<wu>) =
        FloatWuToInt n

    let numCapsFontImageDefinitionFor (FontID(fontIndex)) =
        let fontSet = gameResources.Fonts
        if fontIndex >= 0 && fontIndex < fontSet.Length then
            fontSet.[fontIndex]
        else
            failwith "invalid font resource index"

    let bmpSourceImageFor (ImageID(imageIndex)) =
        let imageSet = gameResources.GameBMPs
        if imageIndex >= 0 && imageIndex < imageSet.Length then
            imageSet.[imageIndex]
        else
            failwith "invalid image resource index"

    match drawingCommand with

        | DrawImageWithTopLeftAtInt(left, top, imageVisual) ->
            DrawImage renderer (bmpSourceImageFor imageVisual) ((int) left) ((int) top) // NB: not truncations, just removing the units of measure

        | DrawStretchedImageWithTopLeftAt(left, top, imageVisual, width, height) ->
            let bmp = bmpSourceImageFor imageVisual
            DrawSubImage renderer bmp.TextureHandle 0 0 bmp.SourceRect.w bmp.SourceRect.h (px left) (px top) (px width) (px height)

        | DrawSubImageStretchedToTarget(srcleft, srctop, srcwidth, srcheight, dstleft, dsttop, dstwidth, dstheight, imageVisual) ->
            let bmp = bmpSourceImageFor imageVisual
            DrawSubImage renderer bmp.TextureHandle srcleft srctop srcwidth srcheight (px dstleft) (px dsttop) (px dstwidth) (px dstheight)

        | DrawCharImageWithTopLeftAt(left, top, charIndex, fontVisual) ->
            let fontDefinition = numCapsFontImageDefinitionFor fontVisual
            let cwid = fontDefinition.CharWidth
            let chei = fontDefinition.CharHeight
            let chx = (int charIndex) * cwid // TODO: constant: assuming char with for fonts.
            DrawSubImage renderer (fontDefinition.FontImage.TextureHandle) chx 0 cwid chei (left |> IntWuToInt) (top |> IntWuToInt) cwid chei

        | DrawFilledRectangle(left, top, width, height, SolidColour(colour)) ->
            let right  = (left + width) |> IntWuToInt
            let bottom = (top + height) |> IntWuToInt
            let left   = left |> IntWuToInt
            let top    = top  |> IntWuToInt
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

    let mutable tickCount = 1u
    
    let GetGameTime () = 
        LanguagePrimitives.Float32WithMeasure<seconds> ((float32 tickCount) / 50.0F)

    let staticGameResources = 
        {
            TankMapsList = tankMapsList
        }

    let mutable gameGlobals = InitialGameGlobals ()
    let mutable screenState = NewStoryboard staticGameResources (GetGameTime ()) // TODO: Ideally don't pass gameResources -- only needed for hacking access to a particular screen.

    // 20ms timer installed so that the main event loop receives 'SDL.SDL_EventType.SDL_USEREVENT' every 20ms (1/50th second)
    let timerID =   // TODO: Push into library?
        SDL.SDL_AddTimer(20u, new SDL.SDL_TimerCallback(TimerCallback), 0n)
            
    if timerID = 0 then
        failwith "Failed to install the gameplay timer."

    let renderFunction = 
        RenderToSdl gameResources renderer

    let keyLeft  = NewMutableKey SDL.SDL_Scancode.SDL_SCANCODE_LEFT 
    let keyRight = NewMutableKey SDL.SDL_Scancode.SDL_SCANCODE_RIGHT
    let keyUp    = NewMutableKey SDL.SDL_Scancode.SDL_SCANCODE_UP   
    let keyDown  = NewMutableKey SDL.SDL_Scancode.SDL_SCANCODE_DOWN 
    let keyFire  = NewMutableKey SDL.SDL_Scancode.SDL_SCANCODE_Z    
    let keyPause = NewMutableKey SDL.SDL_Scancode.SDL_SCANCODE_P

    let keysList =
        [ keyLeft ; keyRight ; keyUp ; keyDown ; keyFire ; keyPause ]

    let ObtainKeyStatesAsImmutableRecord () =
        {
            Left =
                {
                    JustDown  = keyLeft.MutJustPressed
                    Held      = keyLeft.MutHeld
                }
            Right =
                {
                    JustDown = keyRight.MutJustPressed
                    Held     = keyRight.MutHeld
                }
            Up =
                {
                    JustDown = keyUp.MutJustPressed
                    Held     = keyUp.MutHeld
                }
            Down =
                {
                    JustDown = keyDown.MutJustPressed
                    Held     = keyDown.MutHeld
                }
            Fire =
                {
                    JustDown = keyFire.MutJustPressed
                    Held     = keyFire.MutHeld
                }
        }

    let mutable mostRecentImmutableInputEventData = 
        ObtainKeyStatesAsImmutableRecord()

    let HandlePossibleKeyStateChange keyOperationResult =

        // Garbage avoidance scheme.

        match keyOperationResult with
            | KeyStateChanged   -> mostRecentImmutableInputEventData <- ObtainKeyStatesAsImmutableRecord ()
            | NoKeyStateChanged -> ()

    let mutable gamePaused = false

    let HandleFrameAdvanceEvent lastGameTime =

        let gameTime = GetGameTime ()

        if keyPause.MutJustPressed then
            gamePaused <- not gamePaused

        SetRenderTargetToTexture renderer backingTexture

        // DEBUG: force clean the drawing texture.  This may help observe artefacts where tiles don't join.
        // renderFunction (DrawFilledRectangle(0.0F<wu>, 0.0F<wu>, 320.0F<wu>, 256.0F<wu>, SolidColour(0xFF00FFu)))

        RenderStoryboard renderFunction screenState gameTime
        SetRenderTargetToScreen renderer
        RenderCopyToFullTarget renderer backingTexture
        Present renderer

        if not gamePaused then

            let frameElapsedTime =
                gameTime - lastGameTime

            let nextGlobals, nextState = 
                match NextStoryboardState 
                        staticGameResources 
                        gameGlobals 
                        screenState 
                        mostRecentImmutableInputEventData 
                        gameTime 
                        frameElapsedTime with
                    | NextStoryboard nextState -> gameGlobals,nextState
                    | NextStoryboardAndGlobals (nextGlobals,nextState) -> nextGlobals,nextState

            tickCount <- tickCount + 1u
            gameGlobals <- nextGlobals
            screenState <- nextState

        HandlePossibleKeyStateChange (keysList |> ClearKeyJustPressedFlags)
        gameTime


    let mutable event            = new SDL.SDL_Event ()
    let mutable lastGameTime     = 0.0F<seconds>
    let mutable terminateProgram = false

    while terminateProgram = false do
        while (SDL.SDL_WaitEvent (&event)) <> 0 && not terminateProgram do

            let msg = event.``type``

            if msg = SDL.SDL_EventType.SDL_QUIT then 
                terminateProgram <- true

            else if msg = SDL.SDL_EventType.SDL_KEYDOWN then
                HandlePossibleKeyStateChange (HandleKeyDownEvent keysList event.key.keysym.scancode)

            else if msg = SDL.SDL_EventType.SDL_KEYUP then
                HandlePossibleKeyStateChange (HandleKeyUpEvent keysList event.key.keysym.scancode)

            else if msg = SDL.SDL_EventType.SDL_USEREVENT then
                let gameTime = HandleFrameAdvanceEvent lastGameTime
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

[<EntryPoint>]
let main argv =

    match WithSdl2Do GameMain with

        | None -> 
            printfn "Failed to start SDL2 library."   // TODO: Let's not use the STDOUT.
            0

        | Some(n) -> 
            n
