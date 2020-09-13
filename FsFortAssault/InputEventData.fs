module InputEventData

open Input

type InputEventData =
    {
        Left  : InputEventKeyState
        Right : InputEventKeyState
        Up    : InputEventKeyState
        Down  : InputEventKeyState
        Fire  : InputEventKeyState
    }

let InputEventDataWhereNothingIsPressed =
    {
        Left  = InputEventKeyStateWhereNothingIsPressed
        Right = InputEventKeyStateWhereNothingIsPressed
        Up    = InputEventKeyStateWhereNothingIsPressed
        Down  = InputEventKeyStateWhereNothingIsPressed
        Fire  = InputEventKeyStateWhereNothingIsPressed
    }

