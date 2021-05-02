module WebGameFramework

open Fable.Core
open Fable.Core.JsInterop
open Browser.Dom

open StaticResourceSetup
open KeyboardForFramework

open Time
open Geometry
open DrawingShapes
open ImagesAndFonts
open Sounds

open Input
open GameStateManagement
open Screen






// [ ] TODO: If ever the onLoad fails on the javascript side, the continuation will never be called, so the game won't start.

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

// TODO: Function naming:  Improve on 'JSIFsharp' prefix.

/// Initialise the WebGL2 interface functions.  If this is not called at startup, the functions will be undefined.
[<Emit("InitialiseWebGl2Interface($0,$1,$2)")>]
let private JSInitialiseWebGl2Interface 
    (retroScreenWidth:int) 
    (retroScreenHeight:int) 
    (windowTitleText:string) = jsNative


[<Emit("IFsharpImageToTexture($0)")>]
let private JSIFsharpImageToTexture (image:obj) : obj = jsNative


[<Emit("IFsharpDrawRectangle($0,$1,$2,$3,$4,$5,$6)")>]
let private JSIFsharpDrawRectangle 
    (x:int)                    // $0
    (y:int)                    // $1
    (width:int)                // $2
    (height:int)               // $3
    (redf:double)              // $4 // TODO: Can WebGL2 accept RGB as bytes?
    (greenf:double)            // $5 // TODO: Can WebGL2 accept RGB as bytes?
    (bluef:double) = jsNative  // $6 // TODO: Can WebGL2 accept RGB as bytes?

let private DrawFilledRectangle (x:int) (y:int) (w:int) (h:int) (colouru:uint32) =

    // TODO: Can WebGL2 directly accept RGB as bytes?

    let red = (colouru &&& 0xFF0000u) >>> 16
    let grn = (colouru &&& 0x00FF00u) >>>  8
    let blu = (colouru &&& 0x0000FFu)

    let inline f (value:uint32) = ((double) value) / 256.0

    JSIFsharpDrawRectangle x y w h (f red) (f grn) (f blu)


[<Emit("IFsharpDrawImagePortion($0,$1,$2,$3,$4,$5,$6,$7,$8,$9,$10)")>]
let private JSIFsharpDrawImagePortion 
    (tex:obj)                    // $0
    (texWidth:int)               // $1
    (texHeight:int)              // $2
    (srcX:int)                   // $3
    (srcY:int)                   // $4
    (srcWidth:int)               // $5
    (srcHeight:int)              // $6
    (dstX:int)                   // $7
    (dstY:int)                   // $8
    (destWidth:int)              // $9
    (destHeight:int) = jsNative  // $10

let inline private DrawSubImage
    (HostImageRef(tex)) texwidth texheight
    srcleft srctop srcwidth srcheight 
    dstleft dsttop dstwidth dstheight =
        if srcwidth > 0 && srcheight > 0 && dstwidth > 0 && dstheight > 0 then // Avoid firefox exception  TODO: Do we need this with the move to WebGL2, and should this even be done here?
            JSIFsharpDrawImagePortion 
                tex texwidth texheight
                srcleft srctop srcwidth srcheight 
                dstleft dsttop dstwidth dstheight



[<Emit("IFsharpDrawImageOneToOne($0,$1,$2,$3,$4)")>]
let private JSIFsharpDrawImageOneToOne
    (tex:obj)              // $0
    (texWidth:int)         // $1
    (texHeight:int)        // $2
    (dstX:int)             // $3
    (dstY:int) = jsNative  // $4

let inline private DrawImage 
    (HostImageRef(tex)) texwidth texheight x y =
        JSIFsharpDrawImageOneToOne tex texwidth texheight x y



/// A supplementary Javascript function that we made.  TODO: It may not be necessary to even have this in Javascript!
[<Emit("loadImageThenDo($0, $1, $2)")>]
let private LoadImageThenDo
    (fileName:string)
    (needsMagentaColourKey:bool)
    (onCompletionOfLoad:obj -> float -> float -> unit) : unit = jsNative



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

let private LoadImageFileListThenDo fileNameObtainer needsMagentaObtainer continuation resourceList =

    let resizeArray = new ResizeArray<Image>(resourceList |> List.length)

    let rec recurse resourceRecordList =

        match resourceRecordList with
            | [] -> 
                continuation (resizeArray.ToArray())

            | resourceRecord::tail ->

                let fileName = resourceRecord |> fileNameObtainer
                let needsMagentaColourKeying = resourceRecord |> needsMagentaObtainer

                LoadImageThenDo fileName needsMagentaColourKeying (fun htmlImageElement imgWidth imgHeight ->

                    let imgWithHostObject =
                        {
                            ImageMetadata = 
                                {
                                    ImageFileName     = fileName
                                    ImageTransparency = if needsMagentaColourKeying then MagentaColourKeyImage else OpaqueImage
                                    ImageWidth        = (int imgWidth)  |> AsIntEpx
                                    ImageHeight       = (int imgHeight) |> AsIntEpx
                                }
                            HostImageRef = HostImageRef(htmlImageElement |> JSIFsharpImageToTexture)
                        }

                    resizeArray.Add(imgWithHostObject)

                    recurse tail
                )

    recurse resourceList

    // We never get here because the | [] -> match case is the final "what to do next" (need continuation-pass)



let private LoadFontFileListThenDo fileNameObtainer charWidthGetter continuation resourceList =

    let resizeArray = new ResizeArray<Font>(resourceList |> List.length)

    let rec recurse resourceRecordList =

        match resourceRecordList with
            | [] -> 
                continuation (resizeArray.ToArray())

            | resourceRecord::tail ->
                let fileName = resourceRecord |> fileNameObtainer
                let charWidth = resourceRecord |> charWidthGetter

                LoadImageThenDo fileName true (fun htmlImageElement imgWidth imgHeight ->

                    let imgWithHostObject =
                        {
                            ImageMetadata = 
                                {
                                    ImageFileName     = fileName
                                    ImageTransparency = MagentaColourKeyImage
                                    ImageWidth        = (int imgWidth)  |> AsIntEpx
                                    ImageHeight       = (int imgHeight) |> AsIntEpx
                                }
                            HostImageRef = HostImageRef(htmlImageElement |> JSIFsharpImageToTexture)
                        }
                    
                    resizeArray.Add(BasicFont imgWithHostObject charWidth)

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

let InitWebFrameworkThenDo 
        retroScreenSettings 
        (resourceImages:RequestedImage list) 
        (fontResourceImages:RequestedFont list) 
        (resourceSounds:RequestedSound list)
        afterAllLoaded =

    let {
            RetroScreenTitle  = retroScreenTitle
            RetroScreenWidth  = retroScreenWidth 
            RetroScreenHeight = retroScreenHeight
        } = retroScreenSettings

    JSInitialiseWebGl2Interface 
        (retroScreenWidth |> RemoveEpxFromInt)
        (retroScreenHeight |> RemoveEpxFromInt)
        retroScreenTitle

    let imageFileNameGetter requestedImage =
        requestedImage.RequestedImageFileName

    let imageIsColourKeyed requestedImage =
        match requestedImage.RequestedImageTransparency with 
            | OpaqueImage -> false
            | MagentaColourKeyImage -> true

    let fontFileNameGetter  requestedFont = requestedFont.RequestedFontImage.RequestedImageFileName
    let fontCharWidthGetter requestedFont = requestedFont.RequestedFontCharWidth

    let soundFileNameGetter requestedSound =
        requestedSound.RequestedSoundFileName

    fontResourceImages
        |> LoadFontFileListThenDo fontFileNameGetter fontCharWidthGetter
            (fun arrayOfLoadedFonts ->
                resourceImages 
                    |> LoadImageFileListThenDo imageFileNameGetter imageIsColourKeyed 
                        (fun arrayOfLoadedImages ->
                            resourceSounds 
                                |> LoadSoundsFileListThenDo soundFileNameGetter (fun arrayOfLoadedSounds ->
                                    afterAllLoaded arrayOfLoadedImages arrayOfLoadedFonts arrayOfLoadedSounds)
                        )
            )

    // NB: We never get here (continuations called).

// ------------------------------------------------------------------------------------------------------------

let private RenderToWebGL2 drawingCommand =

    match drawingCommand with

        | DrawImageWithTopLeftAtInt(left, top, imageVisual) ->
            DrawImage 
                imageVisual.HostImageRef 
                (imageVisual.ImageMetadata.ImageWidth |> RemoveEpxFromInt)
                (imageVisual.ImageMetadata.ImageHeight |> RemoveEpxFromInt)
                (left |> RemoveEpxFromInt) (top |> RemoveEpxFromInt)

        | DrawStretchedImageWithTopLeftAt(left, top, imageVisual, width, height) ->
            let (texw,texh) = (imageVisual.ImageMetadata.ImageWidth , imageVisual.ImageMetadata.ImageHeight)
            DrawSubImage 
                imageVisual.HostImageRef 
                (texw |> RemoveEpxFromInt)
                (texh |> RemoveEpxFromInt)
                0 0 (texw |> RemoveEpxFromInt) (texh |> RemoveEpxFromInt) 
                (left |> RoundF32EpxToInt) (top |> RoundF32EpxToInt) (width |> RemoveEpxFromInt) (height |> RemoveEpxFromInt)

        | DrawSubImageStretchedToTarget(srcleft, srctop, srcwidth, srcheight, dstleft, dsttop, dstwidth, dstheight, imageVisual) ->
            DrawSubImage 
                imageVisual.HostImageRef 
                (imageVisual.ImageMetadata.ImageWidth |> RemoveEpxFromInt)
                (imageVisual.ImageMetadata.ImageHeight |> RemoveEpxFromInt)
                srcleft srctop srcwidth srcheight 
                (dstleft |> RoundF32EpxToInt) 
                (dsttop |> RoundF32EpxToInt) 
                (dstwidth |> RemoveEpxFromInt) 
                (dstheight |> RemoveEpxFromInt)

        | DrawFilledRectangle(left, top, width, height, SolidColour colour) ->
            let width  = width  |> RemoveEpxFromInt
            let height = height |> RemoveEpxFromInt
            let left   = left   |> RemoveEpxFromInt
            let top    = top    |> RemoveEpxFromInt
            DrawFilledRectangle 
                left top width height 
                colour

// ------------------------------------------------------------------------------------------------------------

let FrameworkWebMain
    listOfKeysNeeded
    gameGlobalStateConstructor
    gameplayStartConstructor
    arrayOfLoadedImages
    arrayOfLoadedFonts
    arrayOfLoadedSounds =

    SetStaticImageAndFontResourceArrays
        arrayOfLoadedImages
        arrayOfLoadedFonts
        arrayOfLoadedSounds

    let gameGlobalState =
        match gameGlobalStateConstructor () with
            | Error msg -> failwith msg
            | Ok globals -> globals

    let gameTime = 0.0<seconds>
    let frameElapsedTime = 0.02<seconds>  // TODO: Revisit parameterisation of frame rate.

    let mutable gameState : ErasedGameState =
        gameplayStartConstructor gameGlobalState gameTime

    let renderFunction = RenderToWebGL2

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

    let mutable tickCount = 0u

    let intervalHandler () =

        tickCount <- tickCount + 1u
                
        let gameTime = 
            (float tickCount) / 50.0 |> InSeconds  // TODO: Revisit parameterisation of frame rate.
                
        gameState.Draw renderFunction gameTime

        let nextGameState = 
            gameState.Frame keyStateGetter gameTime frameElapsedTime 

        nextGameState.Sounds () 
            |> List.iter (fun soundCommand -> 
                match soundCommand with
                    | PlaySoundEffect s -> PlaySound (s.HostSoundRef)
            )

        ClearKeyJustPressedFlags mutableKeyStateStore

        gameState <- nextGameState

    window.setInterval (intervalHandler, 20, ()) |> ignore  // TODO: Revisit parameterisation of frame rate  (20 = 20ms).
    
