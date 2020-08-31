﻿module Geometry

/// F# units-of-measure used to indicate "world" units.
[<Measure>] type wu   // TODO: This is a really bad name because it's really "display units".

let IntWuToInt (x:int<wu>) = int (x)
let IntToIntWu (x:int)     = LanguagePrimitives.Int32WithMeasure x

let FloatWuToInt x = int (x + 0.5F<wu>)
let IntToFloatWu x = LanguagePrimitives.Float32WithMeasure (float32 x)

let FloatWuToIntWu x = x |> FloatWuToInt |> IntToIntWu

/// A cartesian point.
type PointF32 =
    {
        xwf:float32<wu> ; ywf:float32<wu>
    }

/// A movement delta.
type MovementDeltaF32 =
    {
        MovementDeltaX: float32<wu>
        MovementDeltaY: float32<wu>
    }

/// A rectangle in cartesian World-Space.
type RectangleF32 =
    {
        Left:float32<wu> ; Top:float32<wu> ; Right:float32<wu> ; Bottom:float32<wu>
    }

let RectangleWidth  r = r.Right - r.Left
let RectangleHeight r = r.Bottom - r.Top

/// Return true if p1 is located to the left of p2.
let PointToLeftOf  p1 p2 =  p1.xwf < p2.xwf

/// Return true if p1 is located to the right of p2.
let PointToRightOf p1 p2 =  p1.xwf > p2.xwf

/// Return true if a and b are within a given distance from each other.
let IsWithinRangeOf (a:float32<wu>) triggerDistance (b:float32<wu>) =
    (abs (a-b)) < triggerDistance

/// Return true if points a and b are within a given distance from each other.
let IsWithinRegionOf a triggerDistance b =
    let { xwf=ax ; ywf=ay } = a
    let { xwf=bx ; ywf=by } = b
    (abs (ax-bx)) < triggerDistance && (abs (ay-by)) < triggerDistance

/// Return y value given x, where x specifies a point on a line.
let InterpolateLineSegment (x1:float32) (y1:float32) (x2:float32) (y2:float32) (x:float32) =
    let offset = x - x1
    let dx = x2 - x1
    let dy = y2 - y1
    y1 + offset * (dy / dx)

/// Return true if a point lies within a rectangle.
/// This is floating point, and inclusive in the ranges.
let IsPointWithinRectangle rectangle point =
    point.xwf >= rectangle.Left 
        && point.xwf <= rectangle.Right 
        && point.ywf >= rectangle.Top
        && point.ywf <= rectangle.Bottom

/// Returns true if the rectangles r1 and r2 intersect.
let RectangleIntersects r1 r2 =
    not (r1.Left > r2.Right || r1.Right < r2.Left || r1.Top > r2.Bottom || r1.Bottom < r2.Top)

/// Returns a new point that is an existing point translated 
/// in 2D space by the given 2D delta.
let PointMovedByDelta delta point =
    {
        xwf = point.xwf + delta.MovementDeltaX
        ywf = point.ywf + delta.MovementDeltaY
    }

/// Returns a floating point movement delta that, if applied, would 
/// cause a given object located at 'toPoint' to move towards a target
/// located at 'fromPoint'.
let SimpleMovementDeltaToGetTo toPoint speed fromPoint =

    // NB Slightly weird formulae to eliminate floating point wiggle potential

    {
        MovementDeltaX =
            let delta = toPoint.xwf - fromPoint.xwf
            if delta > speed then
                speed
            elif delta < 0.0F<wu> then
                -speed
            else
                0.0F<wu>

        MovementDeltaY =
            let delta = toPoint.ywf - fromPoint.ywf
            if delta > speed then
                speed
            elif delta < 0.0F<wu> then
                -speed
            else
                0.0F<wu>
    }

/// Returns true if the given point lies within any of the rectangles in a list.
let LiesWithinRectangleList rectangleList point =

    rectangleList |> List.exists (fun r -> IsPointWithinRectangle r point)

/// Returns the square of a number.
let inline Squared x = x * x


let inline RangeMap a b c d v =
    let rangeAB = b - a
    let rangeCD = d - c
    let ratio   = rangeCD / rangeAB
    v * ratio + c

