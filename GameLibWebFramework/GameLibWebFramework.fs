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
[<Emit("InitialiseWebGl2Interface()")>]
let private InitialiseWebGl2Interface () = jsNative

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
    (onCompletionOfLoad:obj -> unit) : unit = jsNative



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

let private LoadImageFileListThenDo fileNameObtainer needsMagentaObtainer widthGetter heightGetter continuation resourceList =

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
                            HostImageRef = HostImageRef(htmlImageElement |> JSIFsharpImageToTexture)
                        }

                    resizeArray.Add(imgWithHostObject)

                    recurse tail
                )

    recurse resourceList

    // We never get here because the | [] -> match case is the final "what to do next" (need continuation-pass)



let private LoadFontFileListThenDo fileNameObtainer widthGetter charWidthGetter heightGetter continuation resourceList =

    let resizeArray = new ResizeArray<Font>(resourceList |> List.length)

    let rec recurse resourceRecordList =

        match resourceRecordList with
            | [] -> 
                continuation (resizeArray.ToArray())

            | resourceRecord::tail ->
                let fileName = resourceRecord |> fileNameObtainer
                let w = resourceRecord |> widthGetter
                let h = resourceRecord |> heightGetter
                let charWidth = resourceRecord |> charWidthGetter

                LoadImageThenDo fileName true (fun htmlImageElement ->

                    let imgWithHostObject =
                        {
                            ImageMetadata = 
                                {
                                    ImageFileName  = fileName
                                    ImageTransparency = MagentaColourKeyImage
                                    ImageWidth     = w
                                    ImageHeight    = h
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

let LoadResourceFilesThenDo resourceImages (fontResourceImages:FontMetadata list) resourceSounds afterAllLoaded =

    InitialiseWebGl2Interface ()

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

    let fontFileNameGetter metadata  = metadata.FontImageMetadata.ImageFileName
    let fontWidthGetter metadata     = metadata.FontImageMetadata.ImageWidth
    let fontCharWidthGetter metadata = metadata.FontCharWidth
    let fontHeightGetter metadata    = metadata.FontImageMetadata.ImageHeight

    fontResourceImages
        |> LoadFontFileListThenDo fontFileNameGetter fontWidthGetter fontCharWidthGetter fontHeightGetter
            (fun arrayOfLoadedFonts ->
                resourceImages 
                    |> LoadImageFileListThenDo imageFileNameGetter imageIsColourKeyed imageWidthGetter imageHeightGetter
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
                (imageVisual.ImageMetadata.ImageWidth |> IntEpxToInt)
                (imageVisual.ImageMetadata.ImageHeight |> IntEpxToInt)
                (left |> IntEpxToInt) (top |> IntEpxToInt)

        | DrawStretchedImageWithTopLeftAt(left, top, imageVisual, width, height) ->
            let (texw,texh) = (imageVisual.ImageMetadata.ImageWidth , imageVisual.ImageMetadata.ImageHeight)
            DrawSubImage 
                imageVisual.HostImageRef 
                (texw |> IntEpxToInt)
                (texh |> IntEpxToInt)
                0 0 (texw |> IntEpxToInt) (texh |> IntEpxToInt) 
                (left |> FloatEpxToInt) (top |> FloatEpxToInt) (width |> IntEpxToInt) (height |> IntEpxToInt)

        | DrawSubImageStretchedToTarget(srcleft, srctop, srcwidth, srcheight, dstleft, dsttop, dstwidth, dstheight, imageVisual) ->
            DrawSubImage 
                imageVisual.HostImageRef 
                (imageVisual.ImageMetadata.ImageWidth |> IntEpxToInt)
                (imageVisual.ImageMetadata.ImageHeight |> IntEpxToInt)
                srcleft srctop srcwidth srcheight 
                (dstleft |> FloatEpxToInt) 
                (dsttop |> FloatEpxToInt) 
                (dstwidth |> IntEpxToInt) 
                (dstheight |> IntEpxToInt)

        | DrawFilledRectangle(left, top, width, height, SolidColour colour) ->
            let width  = width  |> IntEpxToInt
            let height = height |> IntEpxToInt
            let left   = left   |> IntEpxToInt
            let top    = top    |> IntEpxToInt
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

    let gameTime = 0.0F<seconds>
    let frameElapsedTime = 0.02F<seconds>  // TODO: Revisit parameterisation of frame rate.

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
            (float32 tickCount) / 50.0F |> InSeconds  // TODO: Revisit parameterisation of frame rate.
                
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
    
