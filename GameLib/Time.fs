module Time

[<Measure>] type seconds

type GameTimeBasis = float
type GameTime = float<seconds>  // TODO: Use 64-bit float

let inline InSeconds s = LanguagePrimitives.FloatWithMeasure<seconds> (s)

let LevelCountAtRate (rate:GameTimeBasis) numberOfSteps (gameTime:GameTime) =
    if numberOfSteps > 0 then
        ((int)((gameTime * rate) + 0.5<seconds>)) % numberOfSteps
    else
        failwith "Cannot count through zero steps"

let EdgeCountAtRate rate numberOfSteps timePerFrame (gameTime:GameTime) =
    let prev  = LevelCountAtRate rate numberOfSteps (gameTime - timePerFrame)
    let index = LevelCountAtRate rate numberOfSteps gameTime
    if prev <> index then ValueSome index else ValueNone

/// Returns an integer value that switches between 0 and 1, at a given rate.
let inline UnitPulse (rate:GameTimeBasis) (gameTime:GameTime) = 
    ((int)((gameTime * rate) + 0.5<seconds>)) &&& 1

/// Returns a value that switches between a given low and high, at a given rate.
let PulseBetween rate low high gameTime =   // TODO: rename to include "Level"?
    if (UnitPulse rate gameTime) = 0 then low else high

let EdgePulseBetween rate first second otherwise timePerFrame gameTime =
    match gameTime |> EdgeCountAtRate rate 2 timePerFrame with
        | ValueNone -> otherwise
        | ValueSome index -> if index=0 then first else second

/// Returns a value from an array, in array order, by rotating through the array as time elapses.
let RotateBetweenGroup (rate:GameTimeBasis) (groupArray:'item array) (gameTime:GameTime) =
    groupArray.[LevelCountAtRate rate groupArray.Length gameTime]

/// Returns a boolean value that switches at a given rate.
let PulseActiveAtRate rate gameTime = 
    (UnitPulse rate gameTime) = 0


