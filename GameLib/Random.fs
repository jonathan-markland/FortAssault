module Random


[<Struct>]
type XorShift32State = XorShift32State of uint32


/// Algorithm "xor" from p. 4 of Marsaglia, "Xorshift RNGs".
/// The state word must be initialized to non-zero!
let XorShift32 currentState =

    let (XorShift32State(x)) = currentState
    let x = x ^^^ (x <<< 13)
    let x = x ^^^ (x >>> 17)
    let x = x ^^^ (x <<< 5)
    XorShift32State x

