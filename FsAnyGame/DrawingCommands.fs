module DrawingCommands

open Geometry
open ResourceFileMetadata

/// Solid colour value.
[<Struct>]
type SolidColour = SolidColour of uint32

/// Drawing commands are communicated from the engine to the concrete renderer.
/// These use engine 'epx' coordinates for positions and sizes, and the renderer is at 
/// liberty to interpret these requests in any way is sees fit, when projecting
/// the image onto the screen.
// [<Struct>]
type DrawingCommand =
    
    | DrawImageWithTopLeftAtInt      
        of leftX_1     : int<epx> 
        *  topY_1      : int<epx> 
        *  img_1       : ImageWithHostObject

    | DrawFilledRectangle             
        of leftX_2     : int<epx> 
        *  topY_2      : int<epx> 
        *  width_2     : int<epx> 
        *  height_2    : int<epx> 
        *  colour_2    : SolidColour

    | DrawStretchedImageWithTopLeftAt 
        of leftX_3     : float32<epx> 
        *  topY_3      : float32<epx> 
        *  img_3       : ImageWithHostObject 
        *  width_3     : float32<epx> 
        *  height_3    : float32<epx>

    | DrawSubImageStretchedToTarget
        of srcleft_4   : int 
        *  srctop_4    : int 
        *  srcwidth_4  : int 
        *  srcheight_4 : int 
        *  dstleft_4   : float32<epx> 
        *  dsttop_4    : float32<epx> 
        *  dstwidth_4  : float32<epx> 
        *  dstheight_4 : float32<epx> 
        *  img_4       : ImageWithHostObject

