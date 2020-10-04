module ResourceFileMetadata

open Geometry

/// Transparency handling for bit-mapped images.
type ImageColourKey = 

    /// The image is fully opaque.
    | NoColourKey 
    
    /// The image is transparent where magenta 0xFF00FF pixels
    /// are fully transparent.
    | MagentaColourKey

/// Image information supplied by the game engine to the host.
type EngineImageMetadata =
    {
        /// The leaf-name of the file from which an image resource originates.
        ImageFileName       : string

        /// Colour key handling indicator for this image.
        ImageColourKey      : ImageColourKey

        /// Width of image in pixels.
        ImageWidth          : int<epx>

        /// Height of image in pixels.
        ImageHeight         : int<epx>
    }

/// Opaque type for referring to a *static* image resource, which can be
/// obtained through the StaticResourceAccess module.
[<Struct>]
type ImageID = ImageID of int

/// Opaque type for referring to a *static* font resource, which can be
/// obtained through the StaticResourceAccess module.
[<Struct>]
type FontID = FontID of int

/// A reference to the host's image object (opaque type).
type HostImageObject = HostImageObject of obj

/// Associates a host image object with the image metadata supplied by the game engine
/// at the time the image was loaded.
type ImageWithHostObject =
    {
        EngineImageMetadata : EngineImageMetadata
        HostImageObject     : HostImageObject
    }

/// Font object, incoporating the image that hosts the font lettering.
type FontWithHostObject =
    {
        FontImageWithHostObject : ImageWithHostObject
        CharWidth               : int
        CharHeight              : int
    }

/// Obtain the dimensions of the given image as integers, which are native.
let inline ImageDimensions imageWithHostObject =
    (imageWithHostObject.EngineImageMetadata.ImageWidth , 
        imageWithHostObject.EngineImageMetadata.ImageHeight)

/// Obtain the dimensions of the given image as floating point.
let inline ImageDimensionsF imageWithHostObject =
    (imageWithHostObject.EngineImageMetadata.ImageWidth |> IntToFloatEpx , 
        imageWithHostObject.EngineImageMetadata.ImageHeight |> IntToFloatEpx)

