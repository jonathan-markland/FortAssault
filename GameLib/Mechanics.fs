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
    | MOMVisibleAtPosition of pos:Point<float32<epx>>
    | MOMDisappeared



let FunctionThatGetsPositionOfStationaryObject
    (pos       : Point<float32<epx>>)
    (startTime : GameTime) 
    (duration  : GameTime) =

    let endTime = startTime + duration

    let getPosAtTimeFunction (atTime:GameTime) =

        if atTime >= startTime then
            if atTime <= endTime then
                MOMVisibleAtPosition (pos)
            else
                MOMDisappeared
        else
            MOMYetToAppear

    getPosAtTimeFunction



let FunctionThatGetsPositionOfMovingObject
    (motionFunction : GameTime -> GameTime -> GameTime)
    (startPos  : Point<float32<epx>>) 
    (endPos    : Point<float32<epx>>)
    (startTime : GameTime) 
    (duration  : GameTime) =

    let durationf32 = (float32) duration

    let dx = (endPos.ptx - startPos.ptx) / durationf32
    let dy = (endPos.pty - startPos.pty) / durationf32

    let getPosAtTimeFunction (atTime:GameTime) =

        let t = atTime - startTime

        if t >= 0.0<seconds> then
            if t <= duration then
                let t = motionFunction t duration
                let x = startPos.ptx + dx * (float32 t)
                let y = startPos.pty + dy * (float32 t)
                MOMVisibleAtPosition ({ptx=x ; pty=y})
            else
                MOMDisappeared
        else
            MOMYetToAppear

    getPosAtTimeFunction




// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Unit-space Motion functions
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Unit-Space is where some external range has been 
/// normalized to a floating point value between 0.0F and 1.0F,
/// where 0.0F maps to the start of the range, and 1.0F the end.
[<Measure>]
type unitspace

let inline InUnitSpace n = LanguagePrimitives.Float32WithMeasure<unitspace> (n)

let inline InReverse (tu:float32<unitspace>) =
    1.0F<unitspace> - tu

let inline SpeedingUp (tu:float32<unitspace>) =
    LanguagePrimitives.Float32WithMeasure<unitspace>( (float32) (tu * tu) )

let inline SlowingDown (tu:float32<unitspace>) =
    tu |> InReverse |> SpeedingUp |> InReverse

/// Use f1 for the first half of the animation period, then use f2.
/// But, lie to f1 and f2 so they think they are being used for a full period.
let inline HalfAndHalf f1 f2 (tu:float32<unitspace>) =
    if tu < 0.5F<unitspace> then
        tu |> (*) 2.0F |> f1
    else 
        tu |> (-) 0.5F<unitspace> |> (*) 2.0F |> f2 |> InReverse




// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 
//  Motion functions
// - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

/// Parmeter 'duration' defines a time period over which an animation will happen.
/// Parameter 't' is the current time offset, within that period.
let inline DoneOverDuration (t:GameTime) (duration:GameTime) (unitSpaceMotionFunction:float32<unitspace> -> float32<unitspace>) =
    let tu = (t / duration) |> (float32) |> InUnitSpace
    let t' = tu |> unitSpaceMotionFunction |> float |> ((*) (float duration)) |> InSeconds
    t'


/// Time 't' is the offset into the duration.
let inline LinearMotion (t:GameTime) (_duration:GameTime) = 
    t


/// Time 't' is the offset into the duration.
let inline SpeedingUpMotion (t:GameTime) (duration:GameTime) =
    SpeedingUp |> DoneOverDuration t duration


/// Time 't' is the offset into the duration.
let inline SlowingDownMotion (t:GameTime) (duration:GameTime) =
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
        PositionGetter  :  (GameTime -> MOMReason)
        StartPosition   :  Point<float32<epx>>
        FinalPosition   :  Point<float32<epx>>
        EndGameTime     :  GameTime
    }



let MechanicsControlledStationaryObject
        (pos       : Point<float32<epx>>)
        (startTime : GameTime) 
        (duration  : GameTime) =
    {
        PositionGetter   = FunctionThatGetsPositionOfStationaryObject pos startTime duration
        StartPosition    = pos
        FinalPosition    = pos
        EndGameTime      = startTime + duration
    }



let MechanicsControlledMovingObject
        motionFunction
        (startPos  : Point<float32<epx>>) 
        (endPos    : Point<float32<epx>>)
        (startTime : GameTime) 
        (duration  : GameTime) =
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

