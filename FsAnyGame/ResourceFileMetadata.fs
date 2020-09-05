module ResourceFileMetadata

open Geometry

/// Colour key handling indicator for a bit-mapped image.
type ImageColourKey = NoColourKey | MagentaColourKey

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

/// Opaque type for referring to a *static* image resource.
[<Struct>]
type ImageID = ImageID of int

/// A reference to the host's image object (opaque type).
type HostImageObject = HostImageObject of obj

/// Combines a host image object onto image metadata supplied by the game engine.
type ImageWithHostObject =
    {
        EngineImageMetadata : EngineImageMetadata
        HostImageObject     : HostImageObject
    }

let ImageDimensions imageWithHostObject =
    (imageWithHostObject.EngineImageMetadata.ImageWidth , 
        imageWithHostObject.EngineImageMetadata.ImageHeight)

let ImageDimensionsF imageWithHostObject =
    (imageWithHostObject.EngineImageMetadata.ImageWidth |> IntToFloatEpx , 
        imageWithHostObject.EngineImageMetadata.ImageHeight |> IntToFloatEpx)

