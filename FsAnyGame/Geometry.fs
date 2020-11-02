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

/// An integer cartesian point in Engine coordinate space.
type PointI32 =
    {
        ptix : int<epx>
        ptiy : int<epx>
    }

/// A movement delta in Engine coordinate space.
type MovementDeltaI32 =
    {
        modix : int<epx>
        modiy : int<epx>
    }

/// A floating-point cartesian point in Engine coordinate space.
type PointF32 =
    {
        ptx : float32<epx>
        pty : float32<epx>
    }

/// A movement delta in Engine coordinate space.
type MovementDeltaF32 =
    {
        modx : float32<epx>
        mody : float32<epx>
    }

/// A floating-point rectangle in Engine coordinate space.
type RectangleF32 =
    {
        Left   : float32<epx>
        Top    : float32<epx>
        Right  : float32<epx>
        Bottom : float32<epx>
    }

// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
    
let RectangleWidth  r = r.Right - r.Left
let RectangleHeight r = r.Bottom - r.Top

/// Return true if p1 is located to the left of p2.
let PointToLeftOf  p1 p2 =  p1.ptx < p2.ptx

/// Return true if p1 is located to the right of p2.
let PointToRightOf p1 p2 =  p1.ptx > p2.ptx

/// Return true if a and b are within a given distance from each other.
let IsWithinRangeOf (a:float32<epx>) triggerDistance (b:float32<epx>) =
    (abs (a-b)) < triggerDistance

/// Return true if points a and b are within a given distance from each other.
let IsWithinRegionOf a triggerDistance b =
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
let PointMovedByDelta delta point =
    {
        ptx = point.ptx + delta.modx
        pty = point.pty + delta.mody
    }

/// Returns a new point that is an existing point translated 
/// in 2D space by the given 2D delta.
let PointI32MovedByDelta delta point =
    {
        ptix = point.ptix + delta.modix
        ptiy = point.ptiy + delta.modiy
    }

/// Returns a floating point movement delta that, if applied, would 
/// cause a given object located at 'toPoint' to move towards a target
/// located at 'fromPoint'.
let SimpleMovementDeltaToGetTo toPoint speed fromPoint =

    // NB Slightly weird formulae to eliminate floating point wiggle potential

    {
        modx =
            let delta = toPoint.ptx - fromPoint.ptx
            if delta > speed then
                speed
            elif delta < 0.0F<epx> then
                -speed
            else
                0.0F<epx>

        mody =
            let delta = toPoint.pty - fromPoint.pty
            if delta > speed then
                speed
            elif delta < 0.0F<epx> then
                -speed
            else
                0.0F<epx>
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

