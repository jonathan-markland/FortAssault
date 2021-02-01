module ImagesAndFonts  // TODO: Why have fonts in here as well?  Separate that out?

open Geometry

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Image
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

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

/// A reference to the host's image object (opaque type).
type HostImageRef = HostImageRef of obj

/// Bitmap image record, used with drawing functions that draw bitmaps.
/// Includes metadata about the image.
type Image =
    {
        ImageMetadata   : ImageMetadata
        HostImageRef    : HostImageRef
    }

/// Obtain the dimensions of the given image as integers, which are native.
let inline ImageDimensions imageWithHostObject =
    (imageWithHostObject.ImageMetadata.ImageWidth , 
        imageWithHostObject.ImageMetadata.ImageHeight)  // TODO: Use Dimensions2D type

/// Obtain the dimensions of the given image as floating point.
let inline ImageDimensionsF imageWithHostObject =
    (imageWithHostObject.ImageMetadata.ImageWidth |> IntToFloatEpx , 
        imageWithHostObject.ImageMetadata.ImageHeight |> IntToFloatEpx)  // TODO: Use Dimensions2D type

let inline ImageDimensionsF_v2 imageWithHostObject =  // TODO: Supercede.
    {
        dimx = imageWithHostObject.ImageMetadata.ImageWidth
        dimy = imageWithHostObject.ImageMetadata.ImageHeight
    }


// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Font
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Font information supplied by the game engine to the host.
type FontMetadata =
    {
        /// The font is stored as a bitmap.
        FontImageMetadata : ImageMetadata

        /// The width of the characters in the font.
        FontCharWidth : int
    }

/// Opaque type for referring to a *static* font resource, which can be
/// obtained through the StaticResourceAccess module.
[<Struct>]
type FontID = FontID of int

/// Font record, used with drawing functions that output text.
/// Incoporates the bitmap image that hosts the font lettering.
type Font =
    {
        FontImage       : Image
        MagnifyX        : int
        MagnifyY        : int
        SrcCharWidth    : int
        SrcCharHeight   : int
        CharWidth       : int
        CharHeight      : int
    }

type TextHAlignment = LeftAlign | CentreAlign | RightAlign
type TextVAlignment = TopAlign  | MiddleAlign | BottomAlign

/// Obtain a basic font record from image resource
let BasicFont fontImage charWidth =

    let charHeight =
        int (fontImage.ImageMetadata.ImageHeight)

    {
        FontImage     = fontImage
        MagnifyX      = 1
        MagnifyY      = 1
        SrcCharWidth  = charWidth
        SrcCharHeight = charHeight
        CharWidth     = charWidth
        CharHeight    = charHeight
    }

/// Obtain a magnified version of an existing font.
let MagnifiedFont dx dy oldFont =

    let charSide =
        int (oldFont.FontImage.ImageMetadata.ImageHeight)

    {
        FontImage     = oldFont.FontImage
        MagnifyX      = dx
        MagnifyY      = dy
        SrcCharWidth  = charSide
        SrcCharHeight = charSide
        CharWidth     = dx * charSide
        CharHeight    = dy * charSide
    }

