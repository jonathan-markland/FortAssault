module Mechanics

open Geometry
open Algorithm
open Time




[<Struct>]
type MOMReason =
    | MOMYetToAppear
    | MOMVisibleAtPosition of pos:PointF32
    | MOMDisappeared



let FunctionThatGetsPositionOfStationaryObject  // TODO Should these really take durations for convenience?
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



let FunctionThatGetsPositionOfMovingObject  // TODO Should these really take durations for convenience?
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



let LinearMotion (t:float32<seconds>) (_:float32<seconds>) = 
    t

let SpeedingUpMotion (t:float32<seconds>) (duration:float32<seconds>) =
    let t = t / duration
    LanguagePrimitives.Float32WithMeasure<seconds>( t * t * float32 duration )

let SlowingDownMotion (t:float32<seconds>) (duration:float32<seconds>) =
    let t = t / duration
    let t' = 1.0F - t
    LanguagePrimitives.Float32WithMeasure<seconds>( (1.0F - (t' * t')) * float32 duration )

let ArcMotion (t:float32<seconds>) (duration:float32<seconds>) =
    let halfDuration = duration
    if t < halfDuration then
        SlowingDownMotion (t * 2.0F) duration
    else 
        (SpeedingUpMotion ((t - halfDuration) * 2.0F) duration) + halfDuration


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

