module RenderProjections

open Geometry
open DrawingShapes

/// Returns a 'render' function that will render everything offset (translated) by an amount.
let OffsetBy (pt:Point<int<epx>>) render =

    let { ptx=ofx ; pty=ofy } = pt

    let renderOffsetFunc drawingShape =
        render 
            (match drawingShape with

            | DrawImageWithTopLeftAtInt (x,y,img) -> 
                DrawImageWithTopLeftAtInt (x+ofx,y+ofy,img)

            | DrawFilledRectangle (x,y,w,h,clr) ->
                DrawFilledRectangle (x+ofx,y+ofy,w,h,clr)

            | DrawStretchedImageWithTopLeftAt (x,y,img,w,h) -> 
                DrawStretchedImageWithTopLeftAt (
                    (ofx + (x |> FloatEpxToIntEpx)) |> IntToFloatEpx,
                    (ofy + (y |> FloatEpxToIntEpx)) |> IntToFloatEpx,
                    img,w,h)

            | DrawSubImageStretchedToTarget (sx,sy,sw,sh,dx,dy,dw,dh,img) ->
                DrawSubImageStretchedToTarget (
                    sx,sy,sw,sh,
                    (ofx + (dx |> FloatEpxToIntEpx)) |> IntToFloatEpx,
                    (ofy + (dy |> FloatEpxToIntEpx)) |> IntToFloatEpx,
                    dw,dh,img)
            )

    renderOffsetFunc
