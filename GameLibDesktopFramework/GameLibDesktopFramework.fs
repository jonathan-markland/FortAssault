/// Framework for SDL2-Desktop games.
module DesktopGameFramework

open System.IO
open SDL2  // TODO: It would be nicer if SDLCover could provide everything.
open SDLCover

open StaticResourceSetup
open KeyboardForFramework

open Time
open Geometry
open DrawingShapes
open ImagesAndFonts
open Sounds
open GameStateManagement

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

type FrameworkGameResourcesRecord =  // TODO: Unify with the javascript version!
    {
        GameBMPs    : Image[]
        Fonts       : Font[]
        Sounds      : Sound[]
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private LoadGameImagesFontsAndSounds 
        (gameResourceImages     : RequestedImage list)
        (gameFontResourceImages : RequestedFont list)
        (gameResourceSounds     : RequestedSound list)
        (renderer               : SdlRendererNativeInt) rootPath =  // TODO: Result error string

    let fromFile transparencyColour name = 

        let fullPath =
            Path.Combine(Path.Combine(rootPath, "Images"), name)
        
        if not (File.Exists(fullPath)) then
            failwith (sprintf "Game could not start because file '%s' is missing." fullPath)

        match LoadFromFileAndPrepareForSdlRenderer renderer fullPath transparencyColour with
            | Some(imageRecord) -> imageRecord
            | None -> failwith (sprintf "Game could not start because file '%s' has invalid content." fullPath)

    let fromSoundFile name = 

        let fullPath =
            Path.Combine(Path.Combine(rootPath, "Sounds"), name)
    
        if not (File.Exists(fullPath)) then
            failwith (sprintf "Game could not start because file '%s' is missing." fullPath)

        match LoadSdlSoundFromFile fullPath with
            | Some(soundNativeInt) -> soundNativeInt
            | None -> failwith (sprintf "Game could not start because file '%s' has invalid content." fullPath)

    let magenta =
        Some({ Red=255uy ; Green=0uy ; Blue=255uy })
    
    let imagesArray =
        gameResourceImages
            |> List.map (fun img -> 
                
                let key = 
                    match img.RequestedImageTransparency with 
                        | OpaqueImage -> None 
                        | MagentaColourKeyImage -> magenta
                
                let fileName = img.RequestedImageFileName
                let hostImageObject = fromFile key fileName

                {
                    ImageMetadata = 
                        {
                            ImageFileName       = fileName
                            ImageTransparency   = img.RequestedImageTransparency
                            ImageWidth          = hostImageObject.SourceRect.w |> AsIntEpx
                            ImageHeight         = hostImageObject.SourceRect.h |> AsIntEpx
                        }

                    HostImageRef = HostImageRef hostImageObject
                })

            |> List.toArray

    let fontsArray =
        gameFontResourceImages 
            |> List.map (fun fnt -> 

                let img = fnt.RequestedFontImage
                let fileName = img.RequestedImageFileName

                let hostImageObject = 
                    fromFile magenta fileName

                let imageWithHostObject =
                    {
                        ImageMetadata =
                            {
                                ImageFileName       = fileName
                                ImageTransparency   = img.RequestedImageTransparency
                                ImageWidth          = hostImageObject.SourceRect.w |> AsIntEpx
                                ImageHeight         = hostImageObject.SourceRect.h |> AsIntEpx
                            }

                        HostImageRef = HostImageRef hostImageObject
                    }

                BasicFont imageWithHostObject (fnt.RequestedFontCharWidth)
            ) 
                |> List.toArray

    let soundsArray =
        gameResourceSounds 
            |> List.map (fun snd -> 
                
                let hostSoundObject = fromSoundFile snd.RequestedSoundFileName

                let soundWithHostObject =
                    {
                        SoundMetadata =
                            {
                                SoundFileName = snd.RequestedSoundFileName
                            }

                        HostSoundRef = HostSoundRef hostSoundObject
                    }

                soundWithHostObject
            ) 
                |> List.toArray

    {
        GameBMPs = imagesArray
        Fonts    = fontsArray
        Sounds   = soundsArray
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Render game drawing command to the screen.
/// Here we are choosing to use a 1:1 mapping from engine 'epx' coordinates onto a 
/// HostRetroScreenWidthPixels x HostRetroScreenHeightPixels pixel SDL surface, but 
/// using Float32 allowing higher resolution positioning if this side supported it(!)
let private RenderToSdl renderer drawingCommand =

    match drawingCommand with

        | DrawImageWithTopLeftAtInt(left, top, imageVisual) ->
            let (HostImageRef(hostImageObject)) = imageVisual.HostImageRef
            DrawSdlImage 
                renderer 
                (hostImageObject :?> SdlImageFileMetadata)
                (left |> RemoveEpxFromInt) 
                (top  |> RemoveEpxFromInt)

        | DrawStretchedImageWithTopLeftAt(left, top, imageVisual, width, height) ->
            let (HostImageRef(hostImageObject)) = imageVisual.HostImageRef
            DrawSdlSubImage 
                renderer 
                (hostImageObject :?> SdlImageFileMetadata).TextureHandle
                0 0 
                (imageVisual.ImageMetadata.ImageWidth  |> RemoveEpxFromInt)
                (imageVisual.ImageMetadata.ImageHeight |> RemoveEpxFromInt)
                (left |> RoundF32EpxToInt) (top |> RoundF32EpxToInt) (width |> RemoveEpxFromInt) (height |> RemoveEpxFromInt)

        | DrawSubImageStretchedToTarget(srcleft, srctop, srcwidth, srcheight, dstleft, dsttop, dstwidth, dstheight, imageVisual) ->
            let (HostImageRef(hostImageObject)) = imageVisual.HostImageRef
            DrawSdlSubImage 
                renderer 
                (hostImageObject :?> SdlImageFileMetadata).TextureHandle
                srcleft srctop srcwidth srcheight
                (dstleft |> RoundF32EpxToInt) (dsttop |> RoundF32EpxToInt) (dstwidth |> RemoveEpxFromInt) (dstheight |> RemoveEpxFromInt)

        | DrawFilledRectangle(left, top, width, height, SolidColour(colour)) ->
            let right  = (left + width) |> RemoveEpxFromInt
            let bottom = (top + height) |> RemoveEpxFromInt
            let left   = left |> RemoveEpxFromInt
            let top    = top  |> RemoveEpxFromInt
            SDLCover.DrawSdlFilledRectangle renderer left top right bottom colour

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let PlaySound (HostSoundRef(soundNativeIntObj)) =
    let { SdlSoundNativeInt = soundNativeInt } = soundNativeIntObj :?> SdlSoundNativeInt
    let channel = SDL2.SDL_mixer.Mix_PlayChannel(-1, soundNativeInt, 0) // TODO: Should we do anything if it fails to play?
    if channel = -1 then
        ()  // TODO:  Was I going to actually do something here???
    else
        ()

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private TimerCallback (interval:uint32) (param:nativeint) : uint32 =  // TODO: Can this go into the SDL library?

    let mutable event = new SDL.SDL_Event()

    event.``type`` <- SDL.SDL_EventType.SDL_USEREVENT
    event.user.code <- 0
    event.user.data1 <- 0n
    event.user.data2 <- 0n

    SDL.SDL_PushEvent(&event) |> ignore
    interval  // We can return 0u to cancel the timer here, or interval to keep it going.

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let private MainLoopProcessing 
    renderer 
    backingTexture 
    gameResources 
    initialGameStateConstructor 
    listOfKeysNeeded =

    SetStaticImageAndFontResourceArrays gameResources.GameBMPs gameResources.Fonts gameResources.Sounds

    let mutable tickCount = 1u
    
    let GetGameTime () = 
        (float tickCount) / 50.0 |> InSeconds     // TODO: Revisit parameterisation of frame rate.

    let mutable gameState : ErasedGameState =
        initialGameStateConstructor (GetGameTime ())

    // 20ms timer installed so that the main event loop receives 'SDL.SDL_EventType.SDL_USEREVENT' every 20ms (1/50th second)
    let timerID =
        SDL.SDL_AddTimer(20u, new SDL.SDL_TimerCallback(TimerCallback), 0n)  //   // TODO: Revisit parameterisation of frame rate.
            
    if timerID = 0 then
        failwith "Failed to install the gameplay timer."

    let renderFunction = 
        RenderToSdl renderer

    let mutableKeyStateStore =
        NewMutableKeyStateStore
            SDL.SDL_Scancode.SDL_SCANCODE_P
            listOfKeysNeeded
    
    let keyStateGetter = 
        LiveKeyStateFrom mutableKeyStateStore

    let HandleFrameAdvanceEvent gameTime lastGameTime =

        SetSdlRenderTargetToTexture renderer backingTexture

        // DEBUG: force clean the drawing texture.  This may help observe artefacts where tiles don't join.
        // renderFunction (DrawFilledRectangle(0.0F<wu>, 0.0F<wu>, 320.0F<wu>, 256.0F<wu>, SolidColour(0xFF00FFu)))

        gameState.Draw renderFunction gameTime
        SetSdlRenderTargetToScreen renderer
        RenderCopyToFullSdlTarget renderer backingTexture
        SdlPresent renderer

        let frameElapsedTime =
            gameTime - lastGameTime  // TODO: Why calculate this.  Web version just passes constant.

        let nextGameState = 
            gameState.Frame
                keyStateGetter
                gameTime
                frameElapsedTime  // TODO: Didn't like passing this really.

        nextGameState.Sounds () 
            |> List.iter (fun soundCommand -> 
                match soundCommand with
                    | PlaySoundEffect s -> PlaySound (s.HostSoundRef)
            )

        tickCount <- tickCount + 1u
        gameState <- nextGameState

        mutableKeyStateStore |> ClearKeyJustPressedFlags


    // Classic main event loop.

    let mutable event            = new SDL.SDL_Event ()
    let mutable lastGameTime     = 0.0<seconds>
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
    (gameResourceImages     : RequestedImage list) 
    (gameFontResourceImages : RequestedFont list) 
    (gameResourceSounds     : RequestedSound list)
    listOfKeysNeeded 
    (gameGlobalStateConstructor : unit -> Result<'gameGlobalState,string>)
    (gameplayStartConstructor   : 'gameGlobalState -> GameTime -> ErasedGameState)
        : string option =

        let runGame () =

            match CreateWindowAndSdlRenderer gameWindowTitleString hostWindowWidthPixels hostWindowHeightPixels with   // TODO: Re-visit window initial size constants

                | None ->
                    Some "Main window and SDL2 renderer could not be created."
        
                | Some(_mainWindow, renderer) ->

                    match CreateRgb8888TextureForSdlRenderer renderer hostRetroScreenWidthPixels hostRetroScreenHeightPixels with

                        | None ->
                            Some "Cannot create an SDL2 texture to store the game screen image."

                        | Some(backingTexture) ->

                            let path = System.IO.Path.GetDirectoryName(System.Reflection.Assembly.GetExecutingAssembly().Location)
                            
                            let gameResources = 
                                LoadGameImagesFontsAndSounds 
                                    gameResourceImages 
                                    gameFontResourceImages 
                                    gameResourceSounds 
                                    renderer 
                                    path   // TODO:  Minor: We don't actually free the imageSet handles.

                            let gameGlobalStateResult = 
                                gameGlobalStateConstructor ()

                            let errorResultToOption result =
                                match result with
                                    | Ok _ -> None
                                    | Error errorMessage -> Some errorMessage

                            gameGlobalStateResult
                                |> Result.map (fun gameGlobalState ->

                                    MainLoopProcessing 
                                        renderer 
                                        backingTexture 
                                        gameResources 
                                        (gameplayStartConstructor gameGlobalState)
                                        listOfKeysNeeded

                                    None
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
                


            

