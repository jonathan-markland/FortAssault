module ImagesAndFonts

open Geometry

type TextHAlignment = LeftAlign | CentreAlign | RightAlign
type TextVAlignment = TopAlign  | MiddleAlign | BottomAlign

/// Transparency handling for bit-mapped images.
type ImageTransparency = 

    /// The image is fully opaque.
    | OpaqueImage 
    
    /// The image is transparent where magenta 0xFF00FF pixels
    /// are fully transparent.
    | MagentaColourKeyImage

/// Image information supplied by the game engine to the host.
type ImageMetadata =
    {
        /// The leaf-name of the file from which an image resource originates.
        ImageFileName       : string

        /// Colour key handling indicator for this image.
        ImageTransparency   : ImageTransparency

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
type HostImageRef = HostImageRef of obj

/// Bitmap image recoed, used with drawing functions that draw bitmaps.
/// Includes metadata about the image.
type Image =
    {
        ImageMetadata   : ImageMetadata
        HostImageRef    : HostImageRef
    }

/// Font record, used with drawing functions that output text.
/// Incoporates the bitmap image that hosts the font lettering.
type Font =
    {
        FontImage    : Image
        CharWidth    : int
        CharHeight   : int
    }

/// Obtain the dimensions of the given image as integers, which are native.
let inline ImageDimensions imageWithHostObject =
    (imageWithHostObject.ImageMetadata.ImageWidth , 
        imageWithHostObject.ImageMetadata.ImageHeight)

/// Obtain the dimensions of the given image as floating point.
let inline ImageDimensionsF imageWithHostObject =
    (imageWithHostObject.ImageMetadata.ImageWidth |> IntToFloatEpx , 
        imageWithHostObject.ImageMetadata.ImageHeight |> IntToFloatEpx)

