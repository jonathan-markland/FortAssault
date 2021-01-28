/// A cover for some things in the the SDL2CS, to give stronger typing from the F# viewpoint.
module SDLCover

open SDL2


/// SDL2 library initialisation handling.
let WithSdl2Do f =
    try
        let initResult = SDL.SDL_Init(SDL.SDL_INIT_TIMER ||| SDL.SDL_INIT_AUDIO)
        if initResult = 0 then

            let flags = SDL2.SDL_image.IMG_InitFlags.IMG_INIT_PNG
            let intFlags = (int) flags;
            let initImageResult = SDL2.SDL_image.IMG_Init(flags)
            if (initImageResult &&& intFlags) = intFlags then

                let audioRate = 22050  // TODO: The client game might want to parameterise the Audio rate
                let audioFormat = SDL.AUDIO_S16SYS
                let audioChannels = 2
                let audioBufferSize = 512  // TODO: The client game might want to parameterise the Audio buffer size
                if (SDL2.SDL_mixer.Mix_OpenAudio(audioRate, audioFormat, audioChannels, audioBufferSize)) = 0 then
                    Some(f ())

                else
                    None // TODO: Could not initialise sound
            else
                None // TODO: Handle closedown for outer SDL library in this case.
        else
            None
    with 
        | :? System.BadImageFormatException ->
            None


/// SDL Window handle.
[<Struct>]
type SdlWindowNativeInt =
    {
        SdlWindowNativeInt: nativeint
    }

/// SDL Surface handle.
[<Struct>]
type SdlSurfaceNativeInt =
    {
        SdlSurfaceNativeInt: nativeint
    }

/// SDL image handle, can now be from BMP or PNG source image files.
[<Struct>]
type SdlBitmapImageFileNativeInt =
    {
        SdlBitmapImageNativeInt : SdlSurfaceNativeInt
    }

/// SDL Texture handle.
[<Struct>]
type SdlTextureNativeInt =
    {
        SdlTextureNativeInt: nativeint
    }

/// SDL Renderer handle.
[<Struct>]
type SdlRendererNativeInt =
    {
        SdlRendererNativeInt: nativeint
    }

/// SDL Sound handle, returned by MIX_LoadWAV()
[<Struct>]
type SdlSoundNativeInt =
    {
        SdlSoundNativeInt: nativeint
    }

/// Red, Green, Blue triple.
type RGB =
    {
        Red:   byte
        Green: byte
        Blue:  byte
    }

/// Extended BMP type supports optional transparency colour.
type ExtendedSdlBitmapImage =
    {
        SdlImageNativeInt   : SdlBitmapImageFileNativeInt
        TransparencyColour  : RGB option
    }



let CreateWindowAndSdlRenderer windowTitle width height =   // TODO: Should we drop back to WithNewMainWindowDo, and separate the creation of the renderer out?

    let windowFlags =
        SDL.SDL_WindowFlags.SDL_WINDOW_SHOWN + 
        SDL.SDL_WindowFlags.SDL_WINDOW_RESIZABLE

    let windowNativeInt =
        SDL.SDL_CreateWindow(windowTitle, 100, 32, width, height, windowFlags)  // TODO: Calculate central position on the screen?

    if windowNativeInt <> 0n then

        let renderFlags =
            SDL.SDL_RendererFlags.SDL_RENDERER_ACCELERATED

        let rendererNativeInt =
            SDL.SDL_CreateRenderer(windowNativeInt, -1, renderFlags)

        if rendererNativeInt <> 0n then
            Some({ SdlWindowNativeInt=windowNativeInt } , { SdlRendererNativeInt=rendererNativeInt })
        else
            SDL.SDL_DestroyWindow(windowNativeInt)
            None

    else
        None



/// Convenient constructor for an SDL_Rect.
let inline ToSdlRect x y w h =
    let mutable r = SDL.SDL_Rect()
    r.x <- x
    r.y <- y
    r.w <- w
    r.h <- h
    r



/// Load an image file, supporting BMP and PNG.
/// Only returns a value if load was successful.
let LoadSdlImageFromFile filePath =
    let handle = SDL2.SDL_image.IMG_Load(filePath)
    if handle = nativeint 0 then
        // let msg = SDL2.SDL.SDL_GetError(); // TODO: Return this.
        None
    else
        Some({ SdlBitmapImageNativeInt = { SdlSurfaceNativeInt = handle } })


/// Load a sound file, supporting WAV and OGG.
/// Only returns a value if load was successful.
let LoadSdlSoundFromFile filePath =
    let handle = SDL2.SDL_mixer.Mix_LoadWAV(filePath)
    if handle = nativeint 0 then
        let msg = SDL2.SDL.SDL_GetError(); // TODO: Return this.
        None
    else
        Some({ SdlSoundNativeInt = handle })


type SdlImageFileMetadata =
    {
        ImageHandle   : SdlBitmapImageFileNativeInt
        TextureHandle : SdlTextureNativeInt
        SourceRect    : SDL.SDL_Rect
    }

let ImagePreparedForSdlRenderer (renderer:SdlRendererNativeInt) (exbmp:ExtendedSdlBitmapImage) =

    let { SdlRendererNativeInt = rendererNativeInt } = renderer
    let { SdlImageNativeInt = { SdlBitmapImageNativeInt = { SdlSurfaceNativeInt = surfaceNativeInt } } ; TransparencyColour = transp } = exbmp

    let t = typeof<SDL.SDL_Surface>
    let s = (System.Runtime.InteropServices.Marshal.PtrToStructure(surfaceNativeInt, t)) :?> SDL.SDL_Surface

    match transp with
        | Some(rgb) ->
            SDL.SDL_SetColorKey(
                surfaceNativeInt, 0x00001000 (*SDL_SRCCOLORKEY*),  // TODO: Is this constant seriously not defined in SDL2CS?
                SDL.SDL_MapRGB(s.format, rgb.Red, rgb.Green, rgb.Blue)) |> ignore
        | None ->
            ()

    let texture = SDL.SDL_CreateTextureFromSurface(rendererNativeInt,surfaceNativeInt)
    if texture <> 0n then
        Some({
            ImageHandle   = { SdlBitmapImageNativeInt = { SdlSurfaceNativeInt = surfaceNativeInt } }
            TextureHandle = { SdlTextureNativeInt = texture }
            SourceRect    = ToSdlRect 0 0 s.w s.h
        })
    else
        None


/// Load image file and prepare it for the renderer as a texture:
let LoadFromFileAndPrepareForSdlRenderer renderer fullPath transparencyColour =

    match LoadSdlImageFromFile fullPath with
        | None -> None
        | Some(bmpNativeInt) -> 
            let exbmp =
                {
                    SdlImageNativeInt = bmpNativeInt
                    TransparencyColour = transparencyColour
                }
            ImagePreparedForSdlRenderer renderer exbmp


/// Draw bitmap image onto a surface at a given position.
let DrawSdlImage renderer image left top =
    let {SdlRendererNativeInt=renderer} = renderer
    let mutable dstRect = ToSdlRect left top image.SourceRect.w image.SourceRect.h
    let mutable srcRect = image.SourceRect
    SDL.SDL_RenderCopy(renderer, image.TextureHandle.SdlTextureNativeInt, &srcRect, &dstRect) |> ignore

/// Draw part of a bitmap image onto a surface at a given position.
let DrawSdlSubImage renderer texture srcleft srctop srcwidth srcheight dstleft dsttop dstwidth dstheight =
    let {SdlRendererNativeInt=renderer} = renderer
    let mutable dstRect = ToSdlRect dstleft dsttop dstwidth dstheight
    let mutable srcRect = ToSdlRect srcleft srctop srcwidth srcheight
    SDL.SDL_RenderCopy(renderer, texture.SdlTextureNativeInt, &srcRect, &dstRect) |> ignore

/// Draw a filled rectangle onto the surface at given position in given colour
let DrawSdlFilledRectangle renderer left top right bottom (colourRGB:uint32) =
    let {SdlRendererNativeInt=renderer} = renderer
    let mutable rect = ToSdlRect left top (right-left) (bottom-top)
    SDL.SDL_SetRenderDrawColor(
        renderer, 
        uint8 (colourRGB >>> 16),
        uint8 (colourRGB >>> 8),
        uint8 colourRGB,
        0xFFuy) |> ignore
    SDL.SDL_RenderFillRect(renderer, &rect) |> ignore



let SetSdlRenderTargetToScreen renderer =
    let { SdlRendererNativeInt=renderer } = renderer
    SDL.SDL_SetRenderTarget(renderer, 0n) |> ignore


let SetSdlRenderTargetToTexture renderer texture =
    let { SdlRendererNativeInt=renderer } = renderer
    let { SdlTextureNativeInt=texture } = texture
    SDL.SDL_SetRenderTarget(renderer, texture) |> ignore


let RenderCopyToFullSdlTarget renderer texture =
    let { SdlRendererNativeInt=renderer } = renderer
    let { SdlTextureNativeInt=texture } = texture
    SDL.SDL_RenderCopy(renderer, texture, 0n, 0n) |> ignore


let SdlPresent renderer =
    let { SdlRendererNativeInt=renderer } = renderer
    SDL.SDL_RenderPresent(renderer)


/// Create an RGB 8888 texture for the given renderer.
/// This implies requiring SDL_TEXTUREACCESS_TARGET.
let CreateRgb8888TextureForSdlRenderer renderer textureWidth textureHeight =

    let backingTexture = 
        { 
            SdlTextureNativeInt = 
                SDL.SDL_CreateTexture(
                    renderer.SdlRendererNativeInt, 
                    SDL.SDL_PIXELFORMAT_RGBA8888, 
                    int SDL.SDL_TextureAccess.SDL_TEXTUREACCESS_TARGET, 
                    textureWidth, 
                    textureHeight) 
        }

    if backingTexture.SdlTextureNativeInt = 0n then
        None
    else
        Some(backingTexture)
    