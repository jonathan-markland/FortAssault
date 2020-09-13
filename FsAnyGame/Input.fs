module Input

/// We are using web browser key codes as a unit of common currency
/// across the desktop and web versions.  The desktop hosts will re-map.
type WebBrowserKeyCode = WebBrowserKeyCode of int


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
