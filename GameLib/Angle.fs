module Angle

[<Measure>] type degrees

/// Type conversion retaining units of measure.
let inline DegreesF32toF64 (theta:float32<degrees>) = LanguagePrimitives.FloatWithMeasure<degrees>   (float theta)

/// Type conversion retaining units of measure.
let inline DegreesF64toF32 (theta:float<degrees>)   = LanguagePrimitives.Float32WithMeasure<degrees> (float32 theta)

