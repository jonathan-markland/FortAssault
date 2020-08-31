module Input

[<Struct>]
type InputEventKeyState =
    {
        JustDown  : bool
        Held      : bool
    }

let InputEventKeyStateWhereNothingIsPressed =
    {
        JustDown  = false
        Held      = false
    }
