module Mechanics

open Geometry
open Algorithm
open Time



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Function generators
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

[<Struct>]
type MOMReason =
    | MOMYetToAppear
    | MOMVisibleAtPosition of pos:PointF32
    | MOMDisappeared



let FunctionThatGetsPositionOfStationaryObject
    (pos       : PointF32)
    (startTime : float32<seconds>) 
    (duration  : float32<seconds>) =

    let endTime = startTime + duration

    let getPosAtTimeFunction (atTime:float32<seconds>) =

        if atTime >= startTime then
            if atTime <= endTime then
                MOMVisibleAtPosition (pos)
            else
                MOMDisappeared
        else
            MOMYetToAppear

    getPosAtTimeFunction



let FunctionThatGetsPositionOfMovingObject
    motionFunction
    (startPos  : PointF32) 
    (endPos    : PointF32)
    (startTime : float32<seconds>) 
    (duration  : float32<seconds>) =

    let dx = (endPos.ptx - startPos.ptx) / duration
    let dy = (endPos.pty - startPos.pty) / duration

    let getPosAtTimeFunction (atTime:float32<seconds>) =

        let t = atTime - startTime

        if t >= 0.0F<seconds> then
            if t <= duration then
                let t = motionFunction t duration
                let x = startPos.ptx + dx * t
                let y = startPos.pty + dy * t
                MOMVisibleAtPosition ({ptx=x ; pty=y})
            else
                MOMDisappeared
        else
            MOMYetToAppear

    getPosAtTimeFunction




// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Unit-space Motion functions
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let inline InReverse (i:float32) =
    1.0F - i

let inline SpeedingUp (i:float32) =
    i * i

let inline SlowingDown (i:float32) =
    i |> InReverse |> SpeedingUp |> InReverse

let inline HalfAndHalf f1 f2 (i:float32) =
    if i < 0.5F then
        i |> (*) 2.0F |> f1
    else 
        i |> (-) 0.5F |> (*) 2.0F |> f2 |> InReverse




// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Motion functions
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

let inline DoneOverDuration (t:float32<seconds>) (duration:float32<seconds>) f =
    let t = t / duration
    LanguagePrimitives.Float32WithMeasure<seconds>( (f t) * float32 duration )


/// Time 't' is the offset into the duration.
let inline LinearMotion (t:float32<seconds>) (_duration:float32<seconds>) = 
    t


/// Time 't' is the offset into the duration.
let inline SpeedingUpMotion (t:float32<seconds>) (duration:float32<seconds>) =
    SpeedingUp |> DoneOverDuration t duration


/// Time 't' is the offset into the duration.
let inline SlowingDownMotion (t:float32<seconds>) (duration:float32<seconds>) =
    SlowingDown |> DoneOverDuration t duration



// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Mechanics Object Model
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

    // After the object completes its motion, it turned out that various clients
    // needed to know the final position.  Also, various clients found it useful
    // to know WHEN the object expired (for list removal).  The plain position-getter
    // was inefficient to determine these things, so I refactored to this, 
    // and simplified the MOMReason too.

type MechanicsObjectModel =
    {
        PositionGetter  :  (float32<seconds> -> MOMReason)
        StartPosition   :  PointF32
        FinalPosition   :  PointF32
        EndGameTime     :  float32<seconds>
    }



let MechanicsControlledStationaryObject
        (pos       : PointF32)
        (startTime : float32<seconds>) 
        (duration  : float32<seconds>) =
    {
        PositionGetter   = FunctionThatGetsPositionOfStationaryObject pos startTime duration
        StartPosition    = pos
        FinalPosition    = pos
        EndGameTime      = startTime + duration
    }



let MechanicsControlledMovingObject
        motionFunction
        (startPos  : PointF32) 
        (endPos    : PointF32)
        (startTime : float32<seconds>) 
        (duration  : float32<seconds>) =
    {
        PositionGetter   = FunctionThatGetsPositionOfMovingObject motionFunction startPos endPos startTime duration
        StartPosition    = startPos
        FinalPosition    = endPos
        EndGameTime      = startTime + duration
    }



let MOMPositionAt gameTime mom =
    match mom.PositionGetter gameTime with
        | MOMVisibleAtPosition(pos) -> pos
        | MOMYetToAppear -> mom.StartPosition
        | MOMDisappeared -> mom.FinalPosition



let IsMOMStillInPlayAt gameTime mechanicsObjectModel =
    gameTime <= mechanicsObjectModel.EndGameTime



let ListWithCompletedMOMsRemoved (momGetter:'item -> MechanicsObjectModel) gameTime (genericList:'item list) =
    genericList |> PlanetSavingListFilter (fun genericItem ->
        genericItem |> momGetter |> IsMOMStillInPlayAt gameTime)

