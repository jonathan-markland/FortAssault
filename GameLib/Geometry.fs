﻿module Geometry

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// F# units-of-measure used to indicate Engine Pixels.
/// This is the units with which the game engine calculates.
[<Measure>] type epx

// - - Units Management Casts - -

/// Apply units to plain 32-bit float.  To be used with care!
let inline AsF32Epx (f:float32) = LanguagePrimitives.Float32WithMeasure<epx> (f)

/// Apply units to plain int.  To be used with care!
let inline AsIntEpx (i:int)      = LanguagePrimitives.Int32WithMeasure<epx> i

/// Apply units after converting int to 32-bit float.  To be used with care!
let inline IntToF32Epx i = i |> (float32) |> AsF32Epx

/// Remove units of measure from int<epx>
let inline RemoveEpxFromInt (i:int<epx>) = int i

// - - Units Management - -

/// Type conversion retaining units of measure.
let inline EpxF32toF64 (f:float32<epx>) = LanguagePrimitives.FloatWithMeasure<epx>   (float f)

/// Type conversion retaining units of measure.
let inline EpxF64toF32 (f:float<epx>)   = LanguagePrimitives.Float32WithMeasure<epx> (float32 f)

/// For calling graphics APIs
let inline RoundF32EpxToInt f = int (f + 0.5F<epx>)

/// For calling graphics APIs
let inline RoundF32EpxToIntEpx f = f |> RoundF32EpxToInt |> AsIntEpx

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// A point in Cartesian space.
type Point<'t> =
    {
        ptx : 't
        pty : 't
    }

/// A movement delta in Cartesian space.
type MovementDelta<'t> =
    {
        modx : 't
        mody : 't
    }

/// A rectangle in Cartesian space.
type Rectangle<'t> =
    {
        Left   : 't
        Top    : 't
        Right  : 't
        Bottom : 't
    }

/// Dimensions of a rectangular area in Cartesian space
type RectDimensions<'t> =
    {
        dimx : 't
        dimy : 't
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let inline InvertVector (p:Point<'t>) = { ptx = -p.ptx ; pty = -p.pty }

let inline ApplyToPoint f (p:Point<'t>) = { ptx = f p.ptx ; pty = f p.pty }

let inline PointMult factor (p:Point<'t>) = { ptx = p.ptx * factor ; pty = p.pty * factor }

let inline RectangleWidth  (r:Rectangle<'t>) = r.Right - r.Left
let inline RectangleHeight (r:Rectangle<'t>) = r.Bottom - r.Top

/// Return true if p1 is located to the left of p2.
let PointToLeftOf  p1 p2 =  p1.ptx < p2.ptx

/// Return true if p1 is located to the right of p2.
let PointToRightOf p1 p2 =  p1.ptx > p2.ptx

/// Return true if a and b are within a given distance from each other.
let inline IsWithinRangeOf a triggerDistance b =
    (abs (a-b)) < triggerDistance

/// Return true if points a and b are within a given distance from each other.
let inline IsWithinRegionOf pointA triggerDistance pointB =
    let { ptx=x1 ; pty=y1 } = pointA
    let { ptx=x2 ; pty=y2 } = pointB
    (abs (x1-x2)) < triggerDistance && (abs (y1-y2)) < triggerDistance

let inline OffsetByOrigin originx originy point =
    let { ptx=x ; pty=y } = point
    { ptx=x + originx ; pty=y + originy }

/// Return y value given x, where x specifies a point on a line.
let InterpolateLineSegment (x1:float32) (y1:float32) (x2:float32) (y2:float32) (x:float32) =
    let offset = x - x1
    let dx = x2 - x1
    let dy = y2 - y1
    y1 + offset * (dy / dx)

/// Return true if a point lies within a rectangle.
/// This is floating point, and inclusive in the ranges.
let IsPointWithinRectangle rectangle point =
    point.ptx >= rectangle.Left 
        && point.ptx <= rectangle.Right 
        && point.pty >= rectangle.Top
        && point.pty <= rectangle.Bottom

/// Returns true if the rectangles r1 and r2 intersect.
let RectangleIntersects r1 r2 =
    not (r1.Left >= r2.Right || r1.Right <= r2.Left || r1.Top >= r2.Bottom || r1.Bottom <= r2.Top)

/// Return the tightest bounding rectangle of the two given rectangles.
let TightestBoundingRectangleOf r1 r2 =
    {
        Left   = min  r1.Left   r2.Left
        Top    = min  r1.Top    r2.Top
        Right  = max  r1.Right  r2.Right
        Bottom = max  r1.Bottom r2.Bottom
    }

/// Returns a new point that is an existing point translated 
/// in 2D space by the given 2D delta.
let inline PointMovedByDelta delta point =
    {
        ptx = point.ptx + delta.modx
        pty = point.pty + delta.mody
    }

/// Returns a new rectangle that is an existing rectangle translated 
/// in 2D space by the given 2D delta.
let inline RectangleMovedByDelta delta r =
    {
        Left   = r.Left   + delta.modx
        Top    = r.Top    + delta.mody
        Right  = r.Right  + delta.modx
        Bottom = r.Bottom + delta.mody
    }

/// Return a new rectangle that is the existing rectangle
/// with a border added around all sides.
let inline InflateRectangle border r =
    {
        Left   = r.Left   - border
        Top    = r.Top    - border
        Right  = r.Right  + border
        Bottom = r.Bottom + border
    }

let inline SquareWithTopLeftAt point side =
    let { ptx=x ; pty=y } = point
    { Left=x ; Top=y ; Right=x+side ; Bottom=y+side }

/// Return a rectangle of given width and height centred about the given point.
/// 'two' needs to be the number 2 in the same type as the point and dimensions.
let inline RectangleCenteredAbout point dims two =  // TODO: Can we avoid passing 'two'?
    let x' = point.ptx - (dims.dimx / two)
    let y' = point.pty - (dims.dimy / two) 
    {
        Left    = x'
        Top     = y'
        Right   = x' + dims.dimx
        Bottom  = y' + dims.dimy
    }

/// Returns a floating point movement delta that, if applied, would 
/// cause a given object located at 'fromPoint' to move towards a target
/// located at 'toPoint'.
let SimpleMovementDeltaToGetTo toPoint speed fromPoint =  // TODO: revisit for generics along with companion

    // NB Slightly weird formulae to eliminate floating point wiggle potential

    let inline delta toN speed fromN =
        let delta = toN - fromN
        if delta > speed then speed elif delta < 0.0F<epx> then -speed else 0.0F<epx>

    {
        modx = delta toPoint.ptx speed fromPoint.ptx
        mody = delta toPoint.pty speed fromPoint.pty
    }

/// Returns an integer movement delta that, if applied, would 
/// cause a given object located at 'fromPoint' to move towards a target
/// located at 'toPoint'.
let SimpleMovementDeltaI32ToGetTo toPoint fromPoint =  // TODO: revisit for generics along with companion

    let inline delta toN fromN =
        let delta = toN - fromN
        if delta >= 1<epx> then 1<epx> elif delta <= -1<epx> then -1<epx> else 0<epx>

    {
        modx = delta toPoint.ptx fromPoint.ptx
        mody = delta toPoint.pty fromPoint.pty
    }

/// Returns true if the given point lies within any of the rectangles in a list.
let LiesWithinRectangleList rectangleList point =
    rectangleList |> List.exists (fun r -> IsPointWithinRectangle r point)

/// Returns true if the given point lies within any of the rectangles reported by the things in the list.
let LiesWithinRectangularThingList getRectangleFrom thingList point =
    thingList |> List.exists (fun thing -> point |> IsPointWithinRectangle (getRectangleFrom thing))

/// Returns the square of a number.
let inline Squared x = x * x


let inline RangeMap a b c d v =
    let rangeAB = b - a
    let rangeCD = d - c
    let ratio   = rangeCD / rangeAB
    v * ratio + c

