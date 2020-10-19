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

let DecodedInput keyStateGetter =
    
    let u = keyStateGetter (WebBrowserKeyCode 38)
    let d = keyStateGetter (WebBrowserKeyCode 40)
    let l = keyStateGetter (WebBrowserKeyCode 37)
    let r = keyStateGetter (WebBrowserKeyCode 39)
    let f = keyStateGetter (WebBrowserKeyCode 90)

    let input =
        {
            Left  = l
            Right = r
            Up    = u
            Down  = d
            Fire  = f
        }

    input
