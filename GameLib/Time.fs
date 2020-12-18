module Time

[<Measure>] type seconds

let inline InSeconds s = LanguagePrimitives.Float32WithMeasure<seconds> (s)

/// Returns an integer value that switches between 0 and 1, at a given rate.
let inline UnitPulse (rate:float32) (gameTime:float32<seconds>) = 
    ((int)(gameTime * rate)) &&& 1

/// Returns a value that switches between a given low and high, at a given rate.
let PulseBetween (rate:float32) low high (gameTime:float32<seconds>) = 
    if (UnitPulse rate gameTime) = 0 then low else high

/// Returns a boolean value that switches at a given rate.
let PulseActiveAtRate (rate:float32) (gameTime:float32<seconds>) = 
    (UnitPulse rate gameTime) = 0
