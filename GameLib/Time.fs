module Time

[<Measure>] type seconds

type GameTime = float32<seconds>  // TODO: Use 64-bit float

let inline InSeconds s = LanguagePrimitives.Float32WithMeasure<seconds> (s)

let LevelCountAtRate (rate:float32) numberOfSteps (gameTime:GameTime) =
    if numberOfSteps > 0 then
        ((int)((gameTime * rate) + 0.5F<seconds>)) % numberOfSteps
    else
        failwith "Cannot count through zero steps"

let EdgeCountAtRate (rate:float32) numberOfSteps timePerFrame (gameTime:GameTime) =
    let prev  = LevelCountAtRate rate numberOfSteps (gameTime - timePerFrame)
    let index = LevelCountAtRate rate numberOfSteps gameTime
    if prev <> index then ValueSome index else ValueNone

/// Returns an integer value that switches between 0 and 1, at a given rate.
let inline UnitPulse (rate:float32) (gameTime:GameTime) = 
    ((int)((gameTime * rate) + 0.5F<seconds>)) &&& 1

/// Returns a value that switches between a given low and high, at a given rate.
let PulseBetween (rate:float32) low high (gameTime:GameTime) =   // TODO: rename to include "Level"?
    if (UnitPulse rate gameTime) = 0 then low else high

let EdgePulseBetween (rate:float32) first second otherwise timePerFrame (gameTime:GameTime) =
    match gameTime |> EdgeCountAtRate rate 2 timePerFrame with
        | ValueNone -> otherwise
        | ValueSome index -> if index=0 then first else second

/// Returns a value from an array, in array order, by rotating through the array as time elapses.
let RotateBetweenGroup (rate:float32) (groupArray:'item array) (gameTime:GameTime) =
    groupArray.[LevelCountAtRate rate groupArray.Length gameTime]

/// Returns a boolean value that switches at a given rate.
let PulseActiveAtRate (rate:float32) (gameTime:GameTime) = 
    (UnitPulse rate gameTime) = 0


