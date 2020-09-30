module Time

[<Measure>] type seconds

let inline InSeconds s = LanguagePrimitives.Float32WithMeasure<seconds> (s)
