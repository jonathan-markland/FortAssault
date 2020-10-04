module FortAssaultWeb

open Fable.Core
open Browser.Dom
open ResourceFiles
open ResourceFileMetadata
open TankMapFileLoader
open Storyboard
open Time
open Geometry
open DrawingCommands
open FortAssaultGlobalState
open KeyboardForFramework
open Input
open EngineEntryPoint



// [ ] TODO: If ever the onLoad fails on the javascript side, the continuation will never be called, so the game won't start.


        // The start command is:    npm start


type JavascriptGraphicResources =
    {
        Fonts  : ImageWithHostObject[]  // TODO: possibly revisit
        Images : ImageWithHostObject[]
    }



// ------------------------------------------------------------------------------------------------------------
//  Fable to Javascript interfacing
// ------------------------------------------------------------------------------------------------------------

[<Emit("console.log($0)")>]
let ConsoleLog (messageText:string) : unit = jsNative



/// Javascript alert() function
[<Emit("alert($0)")>]
let Alert (messageText:string) : unit = jsNative



/// A supplementary Javascript function that we made.  TODO: It may not be necessary to even have this in Javascript!
[<Emit("loadImageThenDo($0, $1, $2)")>]
let LoadImageThenDo
    (htmlImageElement:obj)
    (needsMagentaColourKey:bool)
    (onCompletionOfLoad:obj -> unit) : unit = jsNative



[<Emit("$0.drawImage($1, $2, $3)")>]
let JsDrawImage 
    (context2d:Browser.Types.CanvasRenderingContext2D)
    (htmlImageObject:obj)
    (x:int)
    (y:int) = jsNative

let inline DrawImage context2d (HostImageObject(htmlImageObject)) x y =
    JsDrawImage context2d htmlImageObject x y


[<Emit("$0.drawImage($1, $2, $3, $4, $5, $6, $7, $8, $9)")>]
let JsDrawSubImage 
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

let inline DrawSubImage context2d (HostImageObject(htmlImageObject)) srcleft srctop srcwidth srcheight dstleft dsttop dstwidth dstheight =
    JsDrawSubImage context2d htmlImageObject srcleft srctop srcwidth srcheight dstleft dsttop dstwidth dstheight


[<Emit("$0.fillStyle=$5 ; $0.fillRect($1,$2,$3,$4)")>]
let JsDrawFilledRectangle (context2d:Browser.Types.CanvasRenderingContext2D) (x:int) (y:int) (w:int) (h:int) (colour:string) = jsNative
    
let inline DrawFilledRectangle context2d x y w h (colouru:uint32) =
    let colourStr = "#" + colouru.ToString("x6")
    JsDrawFilledRectangle context2d x y w h colourStr



// ------------------------------------------------------------------------------------------------------------
//  Load resources then start game
// ------------------------------------------------------------------------------------------------------------

let LoadFileListThenDo fileNameObtainer needsMagentaObtainer widthGetter heightGetter continuation resourceList =

    let htmlImageElementResizeArrayForFonts = new ResizeArray<ImageWithHostObject>(GameFontResourceImages.Length)

    let rec recurse resourceRecordList fileNameObtainer needsMagentaObtainer =

        match resourceRecordList with
            | [] -> 
                continuation (htmlImageElementResizeArrayForFonts.ToArray())

            | resourceRecord::tail ->
                let fileName = resourceRecord |> fileNameObtainer
                let needsMagentaColourKeying = resourceRecord |> needsMagentaObtainer
                let w = resourceRecord |> widthGetter
                let h = resourceRecord |> heightGetter

                LoadImageThenDo fileName needsMagentaColourKeying (fun htmlImageElement ->

                    let imgWithHostObject =
                        {
                            EngineImageMetadata = 
                                {
                                    ImageFileName  = fileName
                                    ImageColourKey = if needsMagentaColourKeying then MagentaColourKey else NoColourKey
                                    ImageWidth     = w
                                    ImageHeight    = h
                                }
                            HostImageObject = HostImageObject(htmlImageElement)
                        }

                    htmlImageElementResizeArrayForFonts.Add(imgWithHostObject)

                    recurse tail fileNameObtainer needsMagentaObtainer
                )

    recurse resourceList fileNameObtainer needsMagentaObtainer

    // We never get here because the | [] -> match case is the final "what to do next" (need continuation-pass)

// ------------------------------------------------------------------------------------------------------------

let LoadResourceFilesThenDo afterAllLoaded =

    let imageFileNameGetter metadata =
        metadata.ImageFileName

    let imageIsColourKeyed metadata =
        match metadata.ImageColourKey with 
            | NoColourKey -> false
            | MagentaColourKey -> true

    let imageWidthGetter  metadata = metadata.ImageWidth
    let imageHeightGetter metadata = metadata.ImageHeight

    GameFontResourceImages |> LoadFileListThenDo imageFileNameGetter imageIsColourKeyed imageWidthGetter imageHeightGetter
        (fun arrayOfLoadedFonts ->
            GameResourceImages |> LoadFileListThenDo imageFileNameGetter imageIsColourKeyed imageWidthGetter imageHeightGetter
                (fun arrayOfLoadedImages ->
                    afterAllLoaded arrayOfLoadedFonts arrayOfLoadedImages
                )
        )

    // NB: We never get here (continuations called).

// ------------------------------------------------------------------------------------------------------------

let RenderToWebCanvas (gameResources:JavascriptGraphicResources) (context2d:Browser.Types.CanvasRenderingContext2D) drawingCommand =

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
            let bmp = imageVisual.HostImageObject
            DrawImage context2d bmp ((int) left) ((int) top) // NB: not truncations, just removing the units of measure

        | DrawStretchedImageWithTopLeftAt(left, top, imageVisual, width, height) ->
            let bmp = imageVisual.HostImageObject
            let (w,h) = (imageVisual.EngineImageMetadata.ImageWidth , imageVisual.EngineImageMetadata.ImageHeight)
            DrawSubImage context2d bmp 0 0 (w |> IntEpxToInt) (h |> IntEpxToInt) (px left) (px top) (px width) (px height)
            // DrawFilledRectangle context2d (left |> FloatEpxToInt) (top |> FloatEpxToInt) (width |> FloatEpxToInt) (height |> FloatEpxToInt) 0xFF0000u
            // DrawImage context2d bmp 100 150

        | DrawSubImageStretchedToTarget(srcleft, srctop, srcwidth, srcheight, dstleft, dsttop, dstwidth, dstheight, imageVisual) ->
            let bmp = imageVisual.HostImageObject
            // DrawImage context2d bmp 150 100
            // DrawFilledRectangle context2d (px dstleft) (px dsttop) (px dstwidth) (px dstheight) 0x00CC00u
            DrawSubImage context2d bmp srcleft srctop srcwidth srcheight (px dstleft) (px dsttop) (px dstwidth) (px dstheight)

        | DrawCharImageWithTopLeftAt(left, top, charIndex, fontVisual) ->
            let fontDefinition = numCapsFontImageDefinitionFor fontVisual
            let cwid = 8 // TODO: fontDefinition.CharWidth
            let chei = 8 // TODO: fontDefinition.CharHeight
            let chx = (int charIndex) * cwid // TODO: constant: assuming char with for fonts.
            // DrawImage context2d (fontDefinition.HostImageObject) 0 0
            // DrawFilledRectangle context2d (left |> IntEpxToInt) (top |> IntEpxToInt) 7 7 0x0000FFu
            DrawSubImage context2d (fontDefinition.HostImageObject) chx 0 cwid chei (left |> IntEpxToInt) (top |> IntEpxToInt) cwid chei

        | DrawFilledRectangle(left, top, width, height, SolidColour(colour)) ->
            let width  = width  |> IntEpxToInt
            let height = height |> IntEpxToInt
            let left   = left |> IntEpxToInt
            let top    = top  |> IntEpxToInt
            DrawFilledRectangle context2d left top width height colour

// ------------------------------------------------------------------------------------------------------------

let StartGame arrayOfLoadedFonts arrayOfLoadedImages =

    let javascriptGameResources =
        {
            Fonts    = arrayOfLoadedFonts
            Images   = arrayOfLoadedImages
        }

    StaticResourceSetup.SetStaticImageResourceArray arrayOfLoadedImages

    let canvas = document.getElementById("gameScreen") :?> Browser.Types.HTMLCanvasElement
    let context2d = canvas.getContext("2d") :?> Browser.Types.CanvasRenderingContext2D
   
    match LoadTankBattleSequences () with  // TODO:  Don't want to do this here or in the desktop version.
    
        | Ok tankMapsList ->

            let gameTime         = 0.0F<seconds>
            let gameResources    = { TankMapsList = tankMapsList }
            let storyboard       = NewStoryboard gameResources gameTime
            let renderFunction   = RenderToWebCanvas javascriptGameResources context2d
            let frameElapsedTime = 0.02F<seconds>
            let gameGlobals      = InitialFortAssaultGlobals ()
            
            let mutableKeyStateStore =
                NewMutableKeyStateStore
                    80 // P
                    [
                        (37 , WebBrowserKeyCode 37) // LEFT
                        (39 , WebBrowserKeyCode 39) // RIGHT
                        (38 , WebBrowserKeyCode 38) // UP   
                        (40 , WebBrowserKeyCode 40) // DOWN 
                        (90 , WebBrowserKeyCode 90) // Z    (FIRE)
                    ]

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

            let keyStateGetter = LiveKeyStateFrom mutableKeyStateStore

            let rec mainLoop screenState tickCount () =

                let tickCount = tickCount + 1u
                
                let gameTime = 
                    (float32 tickCount) / 50.0F |> InSeconds
                
                RenderStoryboard renderFunction screenState gameTime

                let screenState = 
                    NextStoryboardState gameResources screenState keyStateGetter gameTime frameElapsedTime 

                ClearKeyJustPressedFlags mutableKeyStateStore

                window.setTimeout((mainLoop screenState tickCount), 20) |> ignore

            mainLoop (struct (storyboard, gameGlobals)) 0u ()

   
        | Error msg -> 
            ConsoleLog msg
    



// ------------------------------------------------------------------------------------------------------------
//  BOOT
// ------------------------------------------------------------------------------------------------------------
   
LoadResourceFilesThenDo StartGame







    
