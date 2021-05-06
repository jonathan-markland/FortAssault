module Time

[<Measure>] type seconds

type GameTimeBasis = float
type GameTime = float<seconds>

let inline InSeconds s = LanguagePrimitives.FloatWithMeasure<seconds> (s)

let LevelTrigger (rate:GameTimeBasis) numberOfSteps (gameTime:GameTime) =
    if numberOfSteps > 0 then
        ((int)((gameTime * rate) + 0.5<seconds>)) % numberOfSteps
    else
        failwith "Cannot count through zero steps"

let EdgeTrigger rate numberOfSteps timePerFrame (gameTime:GameTime) =
    let prev  = LevelTrigger rate numberOfSteps (gameTime - timePerFrame)
    let index = LevelTrigger rate numberOfSteps gameTime
    if prev <> index then ValueSome index else ValueNone

/// Returns an integer value that switches between 0 and 1, at a given rate.
let inline UnitPulse (rate:GameTimeBasis) (gameTime:GameTime) = 
    ((int)((gameTime * rate) + 0.5<seconds>)) &&& 1

/// Returns a value that switches between a given low and high, at a given rate.
let PulseBetween rate low high gameTime =   // TODO: rename to include "Level"?
    if (UnitPulse rate gameTime) = 0 then low else high

let EdgePulseBetween rate first second otherwise timePerFrame gameTime =
    match gameTime |> EdgeTrigger rate 2 timePerFrame with
        | ValueNone -> otherwise
        | ValueSome index -> if index=0 then first else second

/// Returns a value from an array, in array order, by rotating through the array as time elapses.
let RotateBetweenGroup (rate:GameTimeBasis) (groupArray:'item array) (gameTime:GameTime) =
    groupArray.[LevelTrigger rate groupArray.Length gameTime]

/// Returns a boolean value that switches at a given rate.
let PulseActiveAtRate rate gameTime = 
    (UnitPulse rate gameTime) = 0


