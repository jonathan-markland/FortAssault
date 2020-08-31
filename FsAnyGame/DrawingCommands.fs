module DrawingCommands

open Geometry

/// Opaque type for referring to an image resource.
[<Struct>]
type ImageID = ImageID of int

/// Opaque type for referring to a font resource.
[<Struct>]
type FontID = FontID of int

/// Solid colour value.
[<Struct>]
type SolidColour = SolidColour of uint32


/// Drawing commands are communicated from the engine to the concrete renderer.
/// These use World Coordinates for positions and sizes, and the renderer is at 
/// liberty to interpret these requests in any way is sees fit, when projecting
/// the image onto the screen.
// [<Struct>]
type DrawingCommand =
    
    | DrawImageWithTopLeftAtInt      
        of leftX_1     : int<wu> 
        *  topY_1      : int<wu> 
        *  img_1       : ImageID

    | DrawFilledRectangle             
        of leftX_2     : int<wu> 
        *  topY_2      : int<wu> 
        *  width_2     : int<wu> 
        *  height_2    : int<wu> 
        *  colour_2    : SolidColour

    | DrawStretchedImageWithTopLeftAt 
        of leftX_3     : float32<wu> 
        *  topY_3      : float32<wu> 
        *  img_3       : ImageID 
        *  width_3     : float32<wu> 
        *  height_3    : float32<wu>

    | DrawSubImageStretchedToTarget
        of srcleft_4   : int 
        *  srctop_4    : int 
        *  srcwidth_4  : int 
        *  srcheight_4 : int 
        *  dstleft_4   : float32<wu> 
        *  dsttop_4    : float32<wu> 
        *  dstwidth_4  : float32<wu> 
        *  dstheight_4 : float32<wu> 
        *  img_4       : ImageID

    | DrawCharImageWithTopLeftAt      
        of textX_5     : int<wu>
        *  textY_5     : int<wu> 
        *  charIndex_5 : uint32 
        *  font_5      : FontID

