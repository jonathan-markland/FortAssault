module WebGameFramework

open Fable.Core
open Browser.Dom

open StaticResourceSetup
open KeyboardForFramework

open Time
open Geometry
open DrawingShapes
open ImagesAndFonts
open Sounds

open Input
open ScreenHandler



// [ ] TODO: If ever the onLoad fails on the javascript side, the continuation will never be called, so the game won't start.


        // The start command is:    npm start


type JavascriptGraphicResources =
    {
        Fonts  : Font[]
        Images : Image[]
    }



// ------------------------------------------------------------------------------------------------------------
//  General support:   Fable to Javascript interfacing
// ------------------------------------------------------------------------------------------------------------

[<Emit("console.log($0)")>]
let private ConsoleLog (messageText:string) : unit = jsNative



/// Javascript alert() function
[<Emit("alert($0)")>]
let private Alert (messageText:string) : unit = jsNative


// ------------------------------------------------------------------------------------------------------------
//  Image and drawing support:   Fable to Javascript interfacing
// ------------------------------------------------------------------------------------------------------------

/// A supplementary Javascript function that we made.  TODO: It may not be necessary to even have this in Javascript!
[<Emit("loadImageThenDo($0, $1, $2)")>]
let private LoadImageThenDo
    (fileName:string)
    (needsMagentaColourKey:bool)
    (onCompletionOfLoad:obj -> unit) : unit = jsNative



[<Emit("$0.drawImage($1, $2, $3)")>]
let private JsDrawImage 
    (context2d:Browser.Types.CanvasRenderingContext2D)
    (htmlImageObject:obj)
    (x:int)
    (y:int) = jsNative

let inline private DrawImage context2d (HostImageRef(htmlImageObject)) x y =
    JsDrawImage context2d htmlImageObject x y


[<Emit("$0.drawImage($1, $2, $3, $4, $5, $6, $7, $8, $9)")>]
let private JsDrawSubImage 
    (context2d:Browser.Types.CanvasRenderingContext2D) // $0
    (htmlImageObject:obj)                              // $1
    (srcleft:int)                                      // $2
    (srctop:int)                                       // $3
    (srcwidth:int)                                     // $4 
    (srcheight:int)                                    // $5 
    (dstleft:int)                                      // $6
    (dsttop:int)                                       // $7
    (dstwidth:int)                                     // $8 
    (dstheight:int) = jsNative                         // $9 

let inline private DrawSubImage context2d (HostImageRef(htmlImageObject)) srcleft srctop srcwidth srcheight dstleft dsttop dstwidth dstheight =
    if srcwidth > 0 && srcheight > 0 && dstwidth > 0 && dstheight > 0 then // Avoid firefox exception
        JsDrawSubImage context2d htmlImageObject srcleft srctop srcwidth srcheight dstleft dsttop dstwidth dstheight


[<Emit("$0.fillStyle=$5 ; $0.fillRect($1,$2,$3,$4)")>]
let private JsDrawFilledRectangle (context2d:Browser.Types.CanvasRenderingContext2D) (x:int) (y:int) (w:int) (h:int) (colour:string) = jsNative
    
let inline private DrawFilledRectangle context2d x y w h (colouru:uint32) =
    let colourStr = "#" + colouru.ToString("x6")
    JsDrawFilledRectangle context2d x y w h colourStr



// ------------------------------------------------------------------------------------------------------------
//  Sounds:   Fable to Javascript interfacing
// ------------------------------------------------------------------------------------------------------------

// Reference:  https://www.html5rocks.com/en/tutorials/webaudio/intro/

/// A supplementary Javascript function that we made.  TODO: It may not be necessary to even have this in Javascript!
[<Emit("loadSoundThenDo($0, $1)")>]
let private LoadSoundThenDo
    (fileName:string)
    (onCompletionOfLoad:obj -> unit) : unit = jsNative   // obj is Javascript type AudioBuffer


[<Emit("playSound($0)")>]
let private JsPlaySound (audioBuffer:obj) : unit = jsNative   // obj is Javascript type AudioBuffer

let private PlaySound (HostSoundRef(jsAudioBuffer)) =
    JsPlaySound jsAudioBuffer


(* TODO
An important point to note is that on iOS, Apple currently mutes all sound output until the first time a sound is played during a user interaction event - for example, calling playSound() inside a touch event handler. You may struggle with Web Audio on iOS "not working" unless you circumvent this - in order to avoid problems like this, just play a sound (it can even be muted by connecting to a Gain Node with zero gain) inside an early UI event - e.g. "touch here to play".
*)



// ------------------------------------------------------------------------------------------------------------
//  Load resources
// ------------------------------------------------------------------------------------------------------------

let private LoadFileListThenDo fileNameObtainer needsMagentaObtainer widthGetter heightGetter continuation resourceList =

    let resizeArray = new ResizeArray<Image>(resourceList |> List.length)

    let rec recurse resourceRecordList =

        match resourceRecordList with
            | [] -> 
                continuation (resizeArray.ToArray())

            | resourceRecord::tail ->
                let fileName = resourceRecord |> fileNameObtainer
                let needsMagentaColourKeying = resourceRecord |> needsMagentaObtainer
                let w = resourceRecord |> widthGetter
                let h = resourceRecord |> heightGetter

                LoadImageThenDo fileName needsMagentaColourKeying (fun htmlImageElement ->

                    let imgWithHostObject =
                        {
                            ImageMetadata = 
                                {
                                    ImageFileName  = fileName
                                    ImageTransparency = if needsMagentaColourKeying then MagentaColourKeyImage else OpaqueImage
                                    ImageWidth     = w
                                    ImageHeight    = h
                                }
                            HostImageRef = HostImageRef(htmlImageElement)
                        }

                    resizeArray.Add(imgWithHostObject)

                    recurse tail
                )

    recurse resourceList

    // We never get here because the | [] -> match case is the final "what to do next" (need continuation-pass)



let private LoadSoundsFileListThenDo fileNameObtainer continuation resourceList =

    let resizeArray = new ResizeArray<Sound>(resourceList |> List.length)

    let rec recurse resourceRecordList =

        match resourceRecordList with
            | [] -> 
                continuation (resizeArray.ToArray())

            | resourceRecord::tail ->
                let fileName = resourceRecord |> fileNameObtainer

                LoadSoundThenDo fileName (fun hostObject ->

                    let libraryObject =
                        {
                            SoundMetadata = 
                                {
                                    SoundFileName = fileName
                                }
                            HostSoundRef = HostSoundRef(hostObject)
                        }

                    resizeArray.Add(libraryObject)

                    recurse tail
                )

    recurse resourceList

    // We never get here because the | [] -> match case is the final "what to do next" (need continuation-pass)

// ------------------------------------------------------------------------------------------------------------

// TODO: This should not need to be private?
let LoadResourceFilesThenDo resourceImages fontResourceImages resourceSounds afterAllLoaded =

    let soundFileNameGetter metadata =
        metadata.SoundFileName

    let imageFileNameGetter metadata =
        metadata.ImageFileName

    let imageIsColourKeyed metadata =
        match metadata.ImageTransparency with 
            | OpaqueImage -> false
            | MagentaColourKeyImage -> true

    let imageWidthGetter  metadata = metadata.ImageWidth
    let imageHeightGetter metadata = metadata.ImageHeight

    fontResourceImages |> LoadFileListThenDo imageFileNameGetter imageIsColourKeyed imageWidthGetter imageHeightGetter
        (fun arrayOfLoadedFontImages ->

            let arrayOfLoadedFonts = 
                arrayOfLoadedFontImages |> Array.map BasicFont

            resourceImages |> LoadFileListThenDo imageFileNameGetter imageIsColourKeyed imageWidthGetter imageHeightGetter
                (fun arrayOfLoadedImages ->

                    resourceSounds |> LoadSoundsFileListThenDo soundFileNameGetter (fun arrayOfLoadedSounds ->
                    afterAllLoaded arrayOfLoadedImages arrayOfLoadedFonts arrayOfLoadedSounds)
                )
        )

    // NB: We never get here (continuations called).

// ------------------------------------------------------------------------------------------------------------

let private RenderToWebCanvas (context2d:Browser.Types.CanvasRenderingContext2D) drawingCommand =

    match drawingCommand with

        | DrawImageWithTopLeftAtInt(left, top, imageVisual) ->
            DrawImage 
                context2d imageVisual.HostImageRef 
                (left |> IntEpxToInt) (top |> IntEpxToInt)

        | DrawStretchedImageWithTopLeftAt(left, top, imageVisual, width, height) ->
            let (w,h) = (imageVisual.ImageMetadata.ImageWidth , imageVisual.ImageMetadata.ImageHeight)
            DrawSubImage 
                context2d imageVisual.HostImageRef
                0 0 (w |> IntEpxToInt) (h |> IntEpxToInt) 
                (left |> FloatEpxToInt) (top |> FloatEpxToInt) (width |> IntEpxToInt) (height |> IntEpxToInt)

        | DrawSubImageStretchedToTarget(srcleft, srctop, srcwidth, srcheight, dstleft, dsttop, dstwidth, dstheight, imageVisual) ->
            DrawSubImage 
                context2d imageVisual.HostImageRef
                srcleft srctop srcwidth srcheight 
                (dstleft |> FloatEpxToInt) (dsttop |> FloatEpxToInt) (dstwidth |> IntEpxToInt) (dstheight |> IntEpxToInt)

        | DrawFilledRectangle(left, top, width, height, SolidColour colour) ->
            let width  = width  |> IntEpxToInt
            let height = height |> IntEpxToInt
            let left   = left   |> IntEpxToInt
            let top    = top    |> IntEpxToInt
            DrawFilledRectangle 
                context2d 
                left top width height 
                colour

// ------------------------------------------------------------------------------------------------------------

open StaticResourceAccess // TODO: HACK  (remove)

let FrameworkWebMain
    listOfKeysNeeded
    gameGlobalStateConstructor
    gameplayStartConstructor
    arrayOfLoadedImages
    arrayOfLoadedFonts
    arrayOfLoadedSounds =

    SetStaticImageAndFontResourceArrays arrayOfLoadedImages arrayOfLoadedFonts arrayOfLoadedSounds

    PlaySound ((SoundFromID(SoundID 0)).HostSoundRef)  // TODO: hack (remove)

    let canvas = document.getElementById("gameScreen") :?> Browser.Types.HTMLCanvasElement
    let context2d = canvas.getContext("2d") :?> Browser.Types.CanvasRenderingContext2D
   
    let gameGlobalState =
        match gameGlobalStateConstructor () with
            | Error msg -> failwith msg
            | Ok globals -> globals

    let gameTime = 0.0F<seconds>
    let frameElapsedTime = 0.02F<seconds>

    let gameState : ErasedGameState =
        gameplayStartConstructor gameGlobalState gameTime

    let renderFunction = RenderToWebCanvas context2d

    let toKeyTuple (WebBrowserKeyCode k) =
        (k, WebBrowserKeyCode k)

    let toKeyTuples lst =
        lst |> List.map toKeyTuple
            
    let mutableKeyStateStore =
        NewMutableKeyStateStore
            80 // P
            (listOfKeysNeeded |> toKeyTuples)

    let registerKeyHandler eventName handlerFunc =
        document.addEventListener(
            eventName, 
            fun e -> 
                let ke: Browser.Types.KeyboardEvent = downcast e
                if handlerFunc mutableKeyStateStore ((int) ke.keyCode) then e.preventDefault())

    registerKeyHandler "keydown" HandleKeyDownEvent
    registerKeyHandler "keyup"   HandleKeyUpEvent

    document.getElementById("loaderScreen").classList.add("hidden")
    document.getElementById("gameScreen").classList.remove("hidden")

    let keyStateGetter = 
        LiveKeyStateFrom mutableKeyStateStore

    let rec mainLoop (gameState : ErasedGameState) tickCount () =

        let tickCount = tickCount + 1u
                
        let gameTime = 
            (float32 tickCount) / 50.0F |> InSeconds  // TODO: Revisit parameterisation of frame rate.
                
        gameState.Draw renderFunction gameTime

        let nextGameState = 
            gameState.Frame 
                keyStateGetter 
                gameTime 
                frameElapsedTime 

        ClearKeyJustPressedFlags mutableKeyStateStore

        // TODO: The setTimeout 20ms will not account for time taken to calculate
        //       the nextGameState.   I am fudging this with 17ms requested.
        //       See MDN for resolution to request animation frame.
        window.setTimeout((mainLoop nextGameState tickCount), 17) |> ignore   // TODO: Revisit parameterisation of frame rate.

    mainLoop gameState 0u ()
    
