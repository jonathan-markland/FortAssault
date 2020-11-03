module Geometry

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// F# units-of-measure used to indicate Engine Pixels.
/// This is the units with which the game engine calculates.
[<Measure>] type epx

let inline IntEpxToInt (x:int<epx>) = int (x)
let inline IntToIntEpx (x:int)      = LanguagePrimitives.Int32WithMeasure x

let inline FloatEpxToInt x = int (x + 0.5F<epx>)
let inline IntToFloatEpx x = LanguagePrimitives.Float32WithMeasure (float32 x)

let inline FloatEpxToIntEpx x = x |> FloatEpxToInt |> IntToIntEpx

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

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    
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
let inline IsWithinRegionOf a triggerDistance b =
    let { ptx=ax ; pty=ay } = a
    let { ptx=bx ; pty=by } = b
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
    point.ptx >= rectangle.Left 
        && point.ptx <= rectangle.Right 
        && point.pty >= rectangle.Top
        && point.pty <= rectangle.Bottom

/// Returns true if the rectangles r1 and r2 intersect.
let RectangleIntersects r1 r2 =
    not (r1.Left > r2.Right || r1.Right < r2.Left || r1.Top > r2.Bottom || r1.Bottom < r2.Top)

/// Returns a new point that is an existing point translated 
/// in 2D space by the given 2D delta.
let inline PointMovedByDelta delta point =
    {
        ptx = point.ptx + delta.modx
        pty = point.pty + delta.mody
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
let SimpleMovementDeltaI32ToGetTo toPoint speed fromPoint =  // TODO: revisit for generics along with companion

    let inline delta toN speed fromN =
        let delta = toN - fromN
        if delta > speed then speed elif delta < 0<epx> then -speed else 0<epx>

    {
        modx = delta toPoint.ptx speed fromPoint.ptx
        mody = delta toPoint.pty speed fromPoint.pty
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

